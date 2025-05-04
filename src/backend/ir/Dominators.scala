package backend.ir.dominators

import scala.collection.mutable.HashMap

trait Dominators[T]:
  def all: Set[T]
  def entry: T
  def preds: T => Set[T]
  def closest(
      sdoms: HashMap[T, Set[T]],
      block: T,
  ): T

  def immediateDominators: HashMap[T, T] =
    val sdoms = strictDominators
    // it is equivalent to (block, set) <- sdoms
    // but it's funny asf
    for element -> set <- sdoms if !sdoms(element).isEmpty
    yield element -> closest(sdoms, element)

  def strictDominators: HashMap[T, Set[T]] =
    val doms = dominators
    doms.map((b, set) => b -> (set - b))

  def dominators: HashMap[T, Set[T]] =
    val dom: HashMap[T, Set[T]] =
      all.map(element => element -> all).to(HashMap)
    dom(entry) = Set(entry)
    var changed = true
    while changed do
      changed = false
      for element <- all if element != entry do
        var temp = all;

        for pred <- preds(element) do temp = temp.intersect(dom(pred))
        temp += element
        if !dom(element).equals(temp) then
          changed = true
          dom(element) = temp;
    dom

  def domTree: HashMap[T, List[T]] =
    def addToMap[K, V](key: K, value: V, m: HashMap[K, List[V]]) =
      if m.contains(key) then m(key) :+= value
      else m.addOne(key -> List(value))

    val dtree: HashMap[T, List[T]] = HashMap()
    val sdoms                      = strictDominators
    for (element, sds) <- sdoms do
      if !sds.isEmpty then
        if sds.size == 1 then addToMap(sds.head, element, dtree)
        else addToMap(closest(sdoms, element), element, dtree)
    dtree

  def dominationFrontier: HashMap[T, Set[T]] =
    def addToMap[K, V](key: K, value: V, m: HashMap[K, Set[V]]) =
      if m.contains(key) then m(key) = m(key) + value
      else m.addOne(key, Set(value))
    val idom                   = immediateDominators
    val df: HashMap[T, Set[T]] = HashMap()
    for element <- all do
      if idom.contains(element) then
        val elementIDom = idom(element)
        for pred <- preds(element) do
          var holder = pred
          while !holder.equals(elementIDom) do
            addToMap(holder, element, df)
            holder = idom(holder)
    df
