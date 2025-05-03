package frontend.sema.passes

import frontend.sema.{Pass, SemaResult}
import frontend.translationUnit.TranslationUnit
import frontend.diagnostics.Diagnostic
import frontend.ast.VarDecl
import frontend.lex.WithSpan
import frontend.types.*

object GlobalVariableTypeSpecifierCheckPass extends Pass[TranslationUnit, TranslationUnit]:
  override def run(unit: TranslationUnit): SemaResult[TranslationUnit] =
    var diags: List[Diagnostic] = Nil
    val newAst = unit.ast.mapValuesInPlace((n, d) => {
      d match
        case VarDecl(const, name, WithSpan(Type.Undef, span), value) =>
          diags :+= Diagnostic.error(
            span,
            "explicit type specifier is required for all global variables",
          )
          VarDecl(const, name, WithSpan(Type.Invalid, span), value)
        case x => x
    })
    SemaResult(unit, diags)
