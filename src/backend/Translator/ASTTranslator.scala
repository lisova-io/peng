package backend.translator.asttranslator

import backend.IR.IR.*
import frontend.ast.*
import frontend.lex.WithSpan

class Translator:
  def translateFn(f: FnDecl): Fn = ???
  def translateDecl(n: Name, d: Decl) =
    d match
      case f: FnDecl                       => translateFn(f)
      case VarDecl(const, name, tp, value) => ??? // for now we blow up on globals
  def translate(ast: AST): Program =
    ast.foreach((n, decl) => ???)
    ???
