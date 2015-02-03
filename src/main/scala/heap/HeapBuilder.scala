package heap

/**
 *
 */
trait HeapBuilder {
  def singletonHeap[A](element: A)(implicit cmp: A => Ordered[A]): Heap[A]

  private def mergeHeaps[A](heaps: List[Heap[A]])(implicit cmp: A => Ordered[A]): Option[Heap[A]] = heaps match {
    case Nil => None
    case List(fullyMergedList) => Some(fullyMergedList)
    case _ => {
      val partiallyMergedList: List[Heap[A]] = heaps
        .grouped(2)
        .map {
          case List(a, b) => a.merge(b)
          case List(a) => a }
        .toList
      mergeHeaps(partiallyMergedList)
    }
  }

  def fromList[A](list: List[A])(implicit cmp: A => Ordered[A]): Option[Heap[A]] =
    mergeHeaps(list.map(singletonHeap(_)(cmp)))
}
