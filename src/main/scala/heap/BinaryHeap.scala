package heap

/**
 *
 */
object BinaryHeap extends HeapBuilder {
  class BinaryHeap[+A](implicit baseCmp: A => Ordered[A]) extends Heap[A] {
    def empty: Boolean = this match {
      case Empty => true
      case _ => false
    }

    def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): BinaryHeap[B] = this match {
      case Empty => Node(element, 1, Empty, Empty)
      case Node(e, size, left, right) =>
        if (left.size > right.size) {
          val Node(newElement, rightSize, rightLeft, rightRight) = right.insert(element)
          if (newElement < e) Node(newElement, size + 1, left,
            Node(e, rightSize, rightLeft, rightRight))
          else Node(e, size + 1, left, Node(newElement, rightSize, rightLeft, rightRight))
        } else {
          val Node(newElement, leftSize, leftLeft, leftRight) = left.insert(element)
          if (newElement < e) Node(newElement, size + 1, Node(e, leftSize, leftLeft, leftRight),
            right)
          else Node(e, size + 1, Node(newElement, leftSize, leftLeft, leftRight), right)
        }
    }

    def findMin: Option[A] = this match {
      case Empty => None
      case Node(element, _, _, _) => Some(element)
    }

    private def size: Int = this match {
      case Empty => 0
      case Node(e, size, left, right) => size
    }

    def deleteMin: Option[BinaryHeap[A]] = this match {
      case Empty => None
      case Node(element, size, left, right) =>
        val (newElement, newHeap) = this.swap.get
        newHeap match {
          case Empty => Some(Empty)
          case Node(oldRoot, newSize, newLeft, newRight) => Some(
            Node(newElement, newSize, newLeft, newRight).sink.get)
        }
    }

    private def swap: Option[(A, BinaryHeap[A])] = this match {
      case Empty => None
      //TODO above should be unreachable as we're using the function (refactor to raise exception)
      case Node(e, size, Empty, Empty) => Some((e, Empty))
      case Node(e, size, left, right) =>
        if (left.size > right.size) {
          val (newE, newLeft) = left.swap.get
          Some((newE, Node(e, size - 1, newLeft, right)))
        }
        else {
          val (newE, newRight) = right.swap.get
          Some((newE, Node(e, size - 1, left, newRight)))
        }
    }

    def sink: Option[BinaryHeap[A]] = this match {
      case Empty => None
      case Node(e, size, Empty, Empty) => Some(this)
      case Node(e, size, left, Empty) =>
        val Node(leftE, leftSize, leftLeft, leftRight) = left
        if (leftE < e) Some(Node(leftE, size, Node(e, leftSize, leftLeft, leftRight), Empty))
        else Some(this)
      case Node(e, size, Empty, right@Node(rightE, rightSize, rightLeft, rightRight)) =>
        if (e > rightE) Some(
          Node(rightE, size, Empty, Node(e, rightSize, rightLeft, rightRight)))
        else Some(this)
      case Node(e, size, left@Node(leftE, leftSize, leftLeft, leftRight),
      right@Node(rightE, rightSize, rightLeft, rightRight)) =>
        if (e > rightE && rightE > leftE) Some(
          Node(leftE, size, Node(e, leftSize, leftLeft, leftRight).sink.get, right))
        else if (e > leftE && leftE > rightE) Some(
          Node(rightE, size, left, Node(e, rightSize, rightLeft, rightRight).sink.get))
        else if (leftE > e && e > rightE) Some(
          Node(leftE, size, Node(e, leftSize, leftLeft, leftRight).sink.get, right))
        else if (rightE > e && e > leftE) Some(
          Node(rightE, size, left, Node(e, rightSize, rightLeft, rightRight).sink.get))
        else Some(this)
    }

    def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): BinaryHeap[B] = other match {
      case Empty => this
      case Node(e, _, _, _) => other.deleteMin match {
        case None => this // TODO best way to indicate failure here?
          /*
          Note: this should never be executed since deleteMin cannot fail
          when the heap is NonEmpty */
        case Some(newOther) => this.insert(e).merge(newOther)
      }
    }
  }


  case object Empty extends BinaryHeap[Nothing]
  case class Node[A](element: A, size: Int, left: BinaryHeap[A], right: BinaryHeap[A])
    (implicit cmp: A => Ordered[A]) extends BinaryHeap[A]

  override def singletonHeap[A](element: A)(implicit cmp: A => Ordered[A]): BinaryHeap[A] =
    Node(element, 1, Empty, Empty)
}
