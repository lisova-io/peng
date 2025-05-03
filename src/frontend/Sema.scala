package frontend.sema

import frontend.diagnostics.Diagnostic
import frontend.ast.Decl
import frontend.translationUnit.TranslationUnit
import frontend.types.TypeDecl

import frontend.sema.passes.*

class SemaResult[+T](val value: T, val diagnostics: List[Diagnostic]):
  def this(v: T) = this(v, Nil)
  def map[U](fn: T => U): SemaResult[U] = SemaResult(fn(value), diagnostics)
  def flatMap[U](fn: T => SemaResult[U]): SemaResult[U] = {
    val SemaResult(v, d) = fn(value)
    SemaResult(v, diagnostics :++ d)
  }
  def foreach[U](fn: T => U): Unit = {
    fn(value)
    ()
  }
  def andThen[U](that: => SemaResult[U]): SemaResult[U] = {
    val SemaResult(v, d) = that
    SemaResult(v, diagnostics :++ d)
  }

object SemaResult:
  def unapply[T](r: SemaResult[T]): (T, List[Diagnostic]) = (r.value, r.diagnostics)

private trait Pass[T, U]:
  def run(arg: T): SemaResult[U]
  final infix def compose[V](next: Pass[U, V]): Pass[T, V] = PassComposition(this, next)
  final infix def `>>`[V](next: Pass[U, V]): Pass[T, V]    = PassComposition(this, next)

private class PassComposition[T, U, V](first: Pass[T, U], second: Pass[U, V]) extends Pass[T, V]:
  override def run(arg: T): SemaResult[V] = {
    val SemaResult(r1, d1) = first.run(arg)
    val SemaResult(r2, d2) = second.run(r1)
    SemaResult(r2, d1 :++ d2)
  }

trait Sema:
  def run(types: List[TypeDecl], decls: List[Decl]): SemaResult[TranslationUnit]

class DefaultSema extends Sema:
  /* TODO:
    -  FIX STRUCTS, THEY LITERALLY BLOW THE WHOLE THING UP
    -  integer range check
    - `is callable?` check
   */
  final private val passes: Pass[(List[TypeDecl], List[Decl]), TranslationUnit] =
    NameCorrectnessCheckPass
      >> GlobalVariableTypeSpecifierCheckPass
      >> InitializerLoopCheckPass
      >> AssignmentCorrectnessCheckPass
      >> ArgsAmountCheckPass
      >> IntLitTypeDeductionPass
      >> TypePropagationPass
      >> TypeDeductionPass
      >> TypeCheckPass

  def run(types: List[TypeDecl], decls: List[Decl]): SemaResult[TranslationUnit] =
    passes.run(types -> decls)
