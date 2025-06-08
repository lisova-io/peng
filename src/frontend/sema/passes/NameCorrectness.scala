package frontend.sema.passes

import frontend.sema.Pass
import frontend.types.TypeDecl
import frontend.ast.*
import frontend.translationUnit.TranslationUnit
import frontend.translationUnit.Name
import frontend.sema.SemaResult
import frontend.diagnostics.Diagnostic
import frontend.lex.Span
import frontend.diagnostics.Message

import diagnostics.containsErrors

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import frontend.types.StructDecl
import frontend.types.Type

object NameCorrectnessCheckPass extends Pass[(List[TypeDecl], List[Decl]), TranslationUnit] {
  private def check(n: AstNode, names: HashSet[Name]): SemaResult[HashSet[Name]] =
    n match {
      case d @ FnDecl(name, params, rettype, body) =>
        val SemaResult(_, d) = check(body, names + name.value ++ params.map(_._1.value))
        SemaResult(names + name.value, d)
      case d @ VarDecl(const, name, tp, value) =>
        val SemaResult(_, d) = check(value, names)
        SemaResult(names + name.value, d)
      case BlockStmt(block) => {
        val SemaResult(_, d) = block.foldLeft(SemaResult(names))((res, stmt) => {
          val SemaResult(n, d) = check(stmt, res.value)
          SemaResult(n, res.diagnostics :++ d)
        })
        SemaResult(names, d)
      }
      case BoolLitExpr(_) | NumLitExpr(_, _) => SemaResult(names)
      case DeclStmt(decl)                    => check(decl, names)
      case IfStmt(cond, tBr, fBr) =>
        SemaResult(
          names,
          check(cond, names).diagnostics :++ check(tBr, names).diagnostics :++ fBr
            .map(check(_, names).diagnostics)
            .getOrElse(Nil),
        )
      case WhileStmt(cond, body) =>
        SemaResult(names, check(cond, names).diagnostics :++ check(body, names).diagnostics)
      case RetStmt(e)     => check(e, names)
      case UnitRetStmt(_) => SemaResult(names)
      case ExprStmt(e)    => check(e, names)
      case BinExpr(op, lhs, rhs, _) =>
        SemaResult(names, check(lhs, names).diagnostics :++ check(rhs, names).diagnostics)
      case UnaryExpr(op, e) => check(e, names)
      case VarRefExpr(name, _) =>
        SemaResult(
          names,
          if names.contains(name.value) then Nil
          else Diagnostic.error(name.span, s"reference to undefined name `${name.value}`") :: Nil,
        )
      case CallExpr(name, args) => {
        val nameCheck =
          if names.contains(name.value) then Nil
          else Diagnostic.error(name.span, s"reference to undefined name `${name.value}`") :: Nil
        SemaResult(
          names,
          args.foldLeft(nameCheck)((diags, e) => diags :++ check(e, names).diagnostics),
        )
      }
    }

  override def run(arg: (List[TypeDecl], List[Decl])): SemaResult[TranslationUnit] = {
    val (types, decls) = arg
    var diagnostics    = List[Diagnostic]()

    var declNames = HashMap[Name, Span]()
    var typeNames = HashMap[Name, Span]()

    for d <- types ++ decls do {
      val (name, isTypeDecl) = d match
        case FnDecl(name, params, rettype, body) => name -> false
        case VarDecl(const, name, tp, value)     => name -> false
        case StructDecl(name, fields)            => name -> true
      val span = declNames.get(name.value).orElse(typeNames.get(name.value))
      span match
        case Some(span) =>
          diagnostics :+= Diagnostic.error(
            Message(name.span, s"redefinition of `${name.value}`") ::
              Message(span, s"previously defined here")
              :: Nil
          )
        case None if isTypeDecl => typeNames += (name.value, name.span)
        case None               => declNames += (name.value, name.span)
    }

    var names     = HashSet.from(typeNames.keys)
    var typeDecls = HashMap[Name, TypeDecl]()
    for tp <- types
        .filter(t => typeNames.contains(t.getName.value))
        .distinctBy(_.getName)
    do
      var diags: List[Diagnostic] = Nil
      tp match
        case StructDecl(_, fields) =>
          var fieldNames = HashMap[Name, Span]()
          for field <- fields do
            field._2.value match
              case Type.Custom(typeName) =>
                if !names.contains(typeName)
                then
                  diags :+= Diagnostic.error(
                    field._2.span,
                    s"undefined type ${typeName}",
                  )
                if fieldNames.contains(field._1.value)
                then
                  diags :+= Diagnostic.error(
                    Message(field._1.span, s"redefinition of field `${field._1.value}`") ::
                      Message(fieldNames(field._1.value), s"previously defined here")
                      :: Nil
                  )
                else fieldNames += (field._1.value -> field._1.span)
              case _ => fieldNames += (field._1.value -> field._1.span)
      diagnostics :++= diags
      if !diags.containsErrors then typeDecls += (tp.getName.value, tp)

    names = HashSet.from(declNames.keys)
    var ast: AST = HashMap()
    for decl <- decls
        .filter(d => declNames.contains(d.getName.value))
        .distinctBy(_.getName)
    do
      val SemaResult(_, diags) = check(decl, names)
      diagnostics :++= diags
      if !diags.containsErrors then ast += (decl.getName.value, decl)
      else
        ast += (
          decl.getName.value,
          decl match
            case FnDecl(name, params, rettype, body) =>
              FnDecl(name, params, rettype, BlockStmt(Nil))
            case VarDecl(const, name, tp, value) => VarDecl(const, name, tp, null),
        )

    SemaResult(TranslationUnit(typeDecls, ast), diagnostics)
  }
}
