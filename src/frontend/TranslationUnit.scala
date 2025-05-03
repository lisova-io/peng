package frontend
package translationUnit

import ast.AST
import ast.printNode
import types.printTypeDecl
import types.TypeDecl

import scala.collection.mutable.HashMap

type Name = String

class TranslationUnit(
    var types: HashMap[Name, TypeDecl] = HashMap(),
    var ast: AST = HashMap(),
)

def printTranslationUnit(unit: TranslationUnit) =
  for tp <- unit.types.values do printTypeDecl(tp)
  for decl <- unit.ast.values do printNode(decl)
