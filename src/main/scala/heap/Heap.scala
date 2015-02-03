package heap

/**
 *
 */
trait Heap[+A] {
  def empty: Boolean
  def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): Heap[B]
  def findMin: Option[A]
  def deleteMin: Option[Heap[A]]
  def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): Heap[B]
}
