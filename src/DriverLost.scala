package driverlost

import scala.collection.mutable.ArrayBuffer

trait Action[Input, Output, Context]:
  final val dependencies: ArrayBuffer[() => Input] = ArrayBuffer()
  final def depends(dependency: () => Input): Action[Input, Output, Context] =
    dependencies.addOne(dependency)
    this
  def run: Output
  def addContext(context: Context): Action[Input, Output, Context]
  def callback: () => Output = () => run
