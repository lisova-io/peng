package frontend
package types

import translationUnit.Name
import lex.WithSpan
import treePrinter.makeOffset

sealed trait TypeDecl:
  def getName: WithSpan[Name]

sealed case class StructDecl(
    val name: WithSpan[Name],
    val fields: List[(WithSpan[Name], WithSpan[Type])],
) extends TypeDecl:
  override def getName = name

enum Type {
  case Unit
  case Bool
  case I8, U8
  case I16, U16
  case I32, U32
  case I64, U64
  case Custom(val name: Name)
  case Undef
  case Invalid

  override def toString: String =
    this match
      case Unit         => "unit"
      case I8           => "i8"
      case U8           => "u8"
      case I16          => "i16"
      case U16          => "u16"
      case I32          => "i32"
      case U32          => "u32"
      case I64          => "i64"
      case U64          => "u64"
      case Bool         => "bool"
      case Custom(name) => name
      case Undef        => "*UNDEF*"
      case Invalid      => "*INVALID*"

  def isIntegerType: Boolean =
    this match
      case I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 => true
      case _                                           => false
}

object Type:
  def fromString(s: String): Type =
    s match
      case "i8"   => Type.I8
      case "u8"   => Type.U8
      case "i16"  => Type.I16
      case "u16"  => Type.U16
      case "i32"  => Type.I32
      case "u32"  => Type.U32
      case "i64"  => Type.I64
      case "u64"  => Type.U64
      case "bool" => Type.Bool
      case "unit" => Type.Unit
      case s      => Type.Custom(s)

private def printTypeDecl(field: (Name, Type), depth: Int): Unit =
  makeOffset(depth)
  println(s"${field._1}: ${field._2}")

def printTypeDecl(tp: TypeDecl, depth: Int = 0): Unit =
  tp match
    case StructDecl(name, fields) =>
      makeOffset(depth)
      println(s"struct ${name.value}")
      fields.foreach(f => printTypeDecl((f._1.value, f._2.value), depth + 1))
