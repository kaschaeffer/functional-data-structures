package heap

/**
 *
 */
object WeightLeftistHeap extends HeapBuilder {

  class WeightLeftistHeap[+A](implicit cmp: A => Ordered[A]) extends Heap[A] {
    def empty: Boolean = this match {
      case Empty => false
      case _ => true
    }

    def findMin: Option[A] = this match {
      case Empty => None
      case Node(element, _, _, _) => Some(element)
    }

    def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): WeightLeftistHeap[B] = (this, other) match {
      case (Empty, Empty) => Empty
      case (Empty, typedOther@Node(_, _, _, _)) => typedOther
      case (_, Empty) => this
      case (Node(element, weight, left, right), Node(otherElement, otherWeight, otherLeft, otherRight)) =>
            if (otherElement > element)
              if (left.weight >= right.weight + otherWeight) Node(element, weight + otherWeight, left, right.merge(other))
              else Node(element, weight + otherWeight, right.merge(other), left)
            else
              if (otherLeft.weight >= otherRight.weight + weight) Node(otherElement, weight + otherWeight, otherLeft, otherRight.merge(this))
              else Node(otherElement, weight + otherWeight, otherRight.merge(this), otherLeft)
      case _ => this // TODO actually indicate failure
    }


    def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): WeightLeftistHeap[B] =
      merge(Node(element, 1, Empty, Empty))

    def deleteMin: Option[WeightLeftistHeap[A]] = this match {
      case Empty => None
      case Node(element, weight, left, right) => Some(left.merge(right))
    }

    def weight: Int = this match {
      case Empty => 0
      case Node(_, w, _, _) => w
    }
  }

  case object Empty extends WeightLeftistHeap[Nothing]
  case class Node[A](element: A, private val _weight: Int, left: WeightLeftistHeap[A], right: WeightLeftistHeap[A])
    (implicit cmp: A => Ordered[A]) extends WeightLeftistHeap[A]

  override def singletonHeap[A](element: A)(implicit cmp: A => Ordered[A]): WeightLeftistHeap[A] =
    Node(element, 1, Empty, Empty)
}
