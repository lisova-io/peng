package ilist
// Intrusive list with reference to parent.

trait IListNode[T <: IListNode[T, P], P](
    var prev: Option[T],
    var next: Option[T],
    var parent: P
):
  def insertBefore(toInsert: T): T =
    assert(toInsert.next.isEmpty)
    val casted = this.asInstanceOf[T]

    toInsert.next = Some(casted)
    this.prev = Some(toInsert)
    prev.foreach(p => p.next = Some(toInsert))
    casted
  def insert(toInsert: T): T =
    assert(toInsert.prev.isEmpty)
    val casted = this.asInstanceOf[T]
    toInsert.prev = Some(casted)
    toInsert.next = this.next
    this.next.foreach(next => next.prev = Some(toInsert))
    this.next = Some(toInsert)
    casted
  def eject: T =
    val casted = this.asInstanceOf[T]
    this.prev.foreach(prev => prev.next = this.next)
    this.next.foreach(next => next.prev = this.prev)
    this.prev = None
    this.next = None
    casted
  def setParent(parent: P): Unit =
    this.parent = parent
