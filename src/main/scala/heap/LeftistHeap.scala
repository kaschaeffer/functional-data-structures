package heap

/**
 *
 */
object LeftistHeap extends HeapBuilder {
  class LeftistHeap[+A](implicit cmp: A => Ordered[A]) extends Heap[A] {
    def empty: Boolean = this match {
      case Empty => false
      case _ => true
    }

    def findMin: Option[A] = this match {
      case Empty => None
      case Node(element, _, _, _) => Some(element)
    }

    def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): LeftistHeap[B] =
      other match {
        case Empty => this
        case typedOther@Node(otherElement, otherRank, otherLeft, otherRight) => this match {
          case Empty => typedOther
          case Node(element, rank, left, right) =>
            if (otherElement > element) makeNode(element, left, right.merge(other))
            else makeNode(otherElement, otherLeft, otherRight.merge(this))
        }
  //      case _ => throw IllegalArgumentException
      }

    private def makeNode[B](element: B, left: LeftistHeap[B], right: LeftistHeap[B])(implicit cmp: B => Ordered[B]): LeftistHeap[B] =
      if (left.rank < right.rank) Node(element, makeRank(right.rank, left.rank), right, left)
      else Node(element, makeRank(left.rank, right.rank), left, right)

    def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): LeftistHeap[B] =
      merge(Node(element, 1, Empty, Empty))

    def deleteMin: Option[LeftistHeap[A]] = this match {
      case Empty => None
      case Node(element, rank, left, right) => Some(left.merge(right))
    }

    def rank: Int = this match {
      case Empty => 0
      case Node(_, r, _, _) => r
    }

    private def makeRank(leftRank: Int, rightRank: Int): Int = rightRank + 1
  }

  case object Empty extends LeftistHeap[Nothing]
  case class Node[A](element: A, private val _rank: Int, left: LeftistHeap[A], right: LeftistHeap[A])
    (implicit cmp: A => Ordered[A]) extends LeftistHeap[A]

  override def singletonHeap[A](element: A)(implicit cmp: A => Ordered[A]): LeftistHeap[A] =
    Node(element, 1, Empty, Empty)
}
