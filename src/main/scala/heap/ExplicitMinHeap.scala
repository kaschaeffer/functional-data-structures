package heap

/**
 *
 */
object ExplicitMinHeap {

  case class ExplicitMinHeap[+A](min: Option[A], heap: Heap[A]) extends Heap[A] {
    def empty: Boolean = heap.empty

    def findMin: Option[A] = min

    def deleteMin: Option[ExplicitMinHeap[A]] = min match {
      case None => None
      case Some(_) => for {
        newHeap <- heap.deleteMin
      } yield {
        val newMin = newHeap.findMin
        ExplicitMinHeap(newMin, newHeap)
      }
    }

    def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): Heap[B] = other match {
      case ExplicitMinHeap(otherMin, otherHeap) =>
        ExplicitMinHeap(List(min, otherMin).min, heap.merge(otherHeap))
    }

    def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): ExplicitMinHeap[B] = {
      val newMin = this.min match {
        case None => element
        case Some(minValue) => if (element < minValue) element else minValue
      }
      ExplicitMinHeap(Some(newMin), this.heap.insert(element))
    }
  }
}

