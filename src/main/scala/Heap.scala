/**
 * Created by schaeffer on 12/29/14.
 */
sealed trait Heap[+T] {
  def empty: Boolean
  def insert[S >: T](element: S)(implicit cmp: S => Ordered[S]): Heap[S]
  def findMin: Option[T]
  def deleteMin: Option[Heap[T]]

  // TODO refactor merge (issue with types!)
  def merge[S >: T](other: Heap[S])(implicit cmp: S => Ordered[S]): Heap[S]
}

object BinaryHeap {
  class BinaryHeap[+T](implicit cmp: T => Ordered[T]) extends Heap[T] {
    def empty: Boolean = this match {
      case EmptyNode => true
      case _ => false
    }

    def insert[S >: T](element: S)(implicit cmp: S => Ordered[S]): Heap[S] = this match {
      case EmptyNode => Node(element, 1, EmptyNode, EmptyNode)
      case Node(e, size, left, right) =>
        if (left.size < right.size) {
          val Node(newElement, rightSize, rightLeft, rightRight) = right.insert(element)
          if (newElement < e) Node(newElement, size + 1, left,
            Node(e, rightSize, rightLeft, rightRight))
          else Node(e, size, left, Node(newElement, rightSize, rightLeft, rightRight))
        } else {
          val Node(newElement, leftSize, leftLeft, leftRight) = left.insert(element)
          if (newElement < e) Node(newElement, size + 1, Node(e, leftSize, leftLeft, leftRight),
            right)
          else Node(e, size, Node(newElement, leftSize, leftLeft, leftRight), right)
        }
    }

    def findMin: Option[T] = this match {
      case EmptyNode => None
      case Node(element, _, _, _) => Some(element)
    }

    private def size: Int = this match {
      case EmptyNode => 0
      case Node(e, size, left, right) => size
    }

    def deleteMin: Option[BinaryHeap[T]] = this match {
      case EmptyNode => None
      case Node(element, size, left, right) => {
        val (newElement, newHeap) = this.swap.get
        newHeap match {
          case EmptyNode => Some(EmptyNode)
          case Node(oldRoot, newSize, newLeft, newRight) => Some(
            Node(newElement, newSize, newLeft, newRight).sink.get)
        }
      }
    }

    private def swap: Option[(T, BinaryHeap[T])] = this match {
      case EmptyNode => None
      // TODO above should be unreachable as we're using the function (refactor to raise exception)
      case Node(e, size, EmptyNode, EmptyNode) => Some((e, EmptyNode))
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

    def sink: Option[BinaryHeap[T]] = this match {
      case EmptyNode => None
      case Node(e, size, EmptyNode, EmptyNode) => Some(this)
      case Node(e, size, left, EmptyNode) =>
        val Node(leftE, leftSize, leftLeft, leftRight) = left
        if (leftE < e) Some(Node(leftE, size, Node(e, leftSize, leftLeft, leftRight), EmptyNode))
        else Some(this)
      case Node(e, size, EmptyNode, right@Node(rightE, rightSize, rightLeft, rightRight)) =>
        if (e > rightE) Some(
          Node(rightE, size, EmptyNode, Node(e, rightSize, rightLeft, rightRight)))
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
  }


  case object EmptyNode extends BinaryHeap[Nothing]
  case class Node[T](element: T, size: Int, left: BinaryHeap[T], right: BinaryHeap[T])
    (implicit cmp: T => Ordered[T]) extends BinaryHeap[T]

}

object LeftistHeap {
  class LeftistHeap[+T](implicit cmp: T => Ordered[T]) extends Heap[T] {
    def empty: Boolean = this match {
      case Empty => false
      case _ => true
    }

    def findMin: Option[T] = this match {
      case Empty => None
      case Node(element, _, _, _) => Some(element)
    }

    def merge[S >: T](other: LeftistHeap[S])(implicit cmp: S => Ordered[S]): LeftistHeap[S] = this match {
      case Empty => other
      case Node(element, rank, left, right) => {
        other match {
          case Empty => this
          case Node(otherElement, otherRank, otherLeft, otherRight) =>
            if (otherElement > element) makeNode(element, left, right.merge(other))
            else makeNode(otherElement, otherLeft, otherRight.merge(this))
        }
      }
    }

    private def makeNode[S](element: S, left: LeftistHeap[S], right: LeftistHeap[S])(implicit cmp: S => Ordered[S]): LeftistHeap[S] =
      if (left.rank < right.rank) Node(element, makeRank(right.rank, left.rank), right, left)
      else Node(element, makeRank(left.rank, right.rank), left, right)

    def insert[S >: T](element: S)(implicit cmp: S => Ordered[S]): LeftistHeap[S] =
      merge(Node(element, 1, Empty, Empty))

    def deleteMin: Option[LeftistHeap[T]] = this match {
      case Empty => None
      case Node(element, rank, left, right) => Some(left.merge(right))
    }

    def rank: Int = this match {
      case Empty => 0
      case Node(_, r, _, _) => r
    }

    private def makeRank(leftRank: Int, rightRank: Int): Int = rightRank + 1
  }

  def fromList[T](elements: List[T])(implicit cmp: T => Ordered[T]): LeftistHeap[T] =
    mergeList(elements map (x => Node(x, 1, Empty, Empty))) match {
      case Nil => Empty
      case x::_ => x
  }

  private def mergeList[T](heaps: List[LeftistHeap[T]])(implicit cmp: T => Ordered[T]): List[LeftistHeap[T]] =
    heaps match {
      case Nil => Nil
      case x::Nil => List(x)
      case x::y::xs => mergeList(x.merge(y)::mergeList(xs))
  }

  case object Empty extends LeftistHeap[Nothing]
  case class Node[T](element: T, private val _rank: Int, left: LeftistHeap[T], right: LeftistHeap[T])
    (implicit cmp: T => Ordered[T]) extends LeftistHeap[T]
}

object WeightLeftistHeap {
  class WeightLeftistHeap[+T](implicit cmp: T => Ordered[T]) extends Heap[T] {
    def empty: Boolean = this match {
      case Empty => false
      case _ => true
    }

    def findMin: Option[T] = this match {
      case Empty => None
      case Node(element, _, _, _) => Some(element)
    }

    def merge[S >: T](other: WeightLeftistHeap[S])(implicit cmp: S => Ordered[S]): WeightLeftistHeap[S] = this match {
      case Empty => other
      case Node(element, weight, left, right) => {
        other match {
          case Empty => this
          case Node(otherElement, otherWeight, otherLeft, otherRight) =>
            if (otherElement > element)
              if (left.weight >= right.weight + otherWeight) Node(element, weight + otherWeight, left, right.merge(other))
              else Node(element, weight + otherWeight, right.merge(other), left)
            else
              if (otherLeft.weight >= otherRight.weight + weight) Node(otherElement, weight + otherWeight, otherLeft, otherRight.merge(this))
              else Node(otherElement, weight + otherWeight, otherRight.merge(this), otherLeft)
        }
      }
    }

    def insert[S >: T](element: S)(implicit cmp: S => Ordered[S]): WeightLeftistHeap[S] =
      merge(Node(element, 1, Empty, Empty))

    def deleteMin: Option[WeightLeftistHeap[T]] = this match {
      case Empty => None
      case Node(element, weight, left, right) => Some(left.merge(right))
    }

    def weight: Int = this match {
      case Empty => 0
      case Node(_, w, _, _) => w
    }
  }

  def fromList[T](elements: List[T])(implicit cmp: T => Ordered[T]): WeightLeftistHeap[T] =
    mergeList(elements map (x => Node(x, 1, Empty, Empty))) match {
      case Nil => Empty
      case x::_ => x
    }

  private def mergeList[T](heaps: List[WeightLeftistHeap[T]])(implicit cmp: T => Ordered[T]): List[WeightLeftistHeap[T]] =
    heaps match {
      case Nil => Nil
      case x::Nil => List(x)
      case x::y::xs => mergeList(x.merge(y)::mergeList(xs))
    }

  case object Empty extends WeightLeftistHeap[Nothing]
  case class Node[T](element: T, private val _weight: Int, left: WeightLeftistHeap[T], right: WeightLeftistHeap[T])
    (implicit cmp: T => Ordered[T]) extends WeightLeftistHeap[T]
}

object BinomialHeap {
  sealed trait BinomialTree[T]
  case class Node[T](element: T, rank: Int, children: List[BinomialTree[T]]) extends BinomialTree[T]


  type BinomialHeap[T] = List[BinomialTree[T]]

  def empty[T](heap: BinomialHeap[T]): Boolean = heap match {
    case Nil => false
    case _ => true
  }

  def link[T](heap1: BinomialTree[T], heap2: BinomialTree[T])(implicit cmp: T => Ordered[T]): Option[BinomialTree[T]] =
    (heap1, heap2) match {
      case (Node(e1, r1, children1), Node(e2, r2, children2)) =>
        if (e1 < e2 && r1 == r2) Some(Node(e1, r1 + 1, heap2::children1))
        else if (e1 <= e2 && r1 == r2) Some(Node(e2, r2 + 1, heap1::children2))
        else None
  }

  def merge[T](heap1: BinomialHeap[T], heap2: BinomialHeap[T]): BinomialHeap[T] = (heap1, heap2) match {
    case (Nil, _) => heap2
    case (_, Nil) => heap1
    case ((tree1@Node(e1, r1, children1))::rest1, (tree2@Node(e2, r2, children2))::rest2) =>
      if (r1 < r2) tree1::merge(rest1, heap2)
      else if (r2 < r1) tree2::merge(rest2, heap1)
      else merge(merge(List(link(tree1, tree2).get), rest1), rest2)
  }

  def insert[T](heap: BinomialHeap[T], element: T)(implicit cmp: T => Ordered[T]): BinomialHeap[T] =
    merge(heap, List(Node(element, 1, Nil)))

  def findMin[T](heap: BinomialHeap[T]): Option[T] = for {
    (Node(element, _, _), _) <- removeMinTree(heap)
  } yield element

  def removeMinTree[T](heap: BinomialHeap[T])(implicit cmp: T => Ordered[T]): Option[(BinomialTree[T], BinomialHeap[T])] = heap match {
    case Nil => None
    case List(x) => Some(x, Nil)
    case (x@Node(e, _, _))::xs =>
      for {
        (xsTree@Node(exs, _, _), xsRest) <- removeMinTree(xs)
      } yield {
        if (e < exs) (x, xs)
        else (xsTree, xsRest)
      }
  }

  def removeMin[T](heap: BinomialHeap[T]): Option[BinomialHeap[T]] = for {
      (Node(e, _, children), newHeap) <- removeMinTree(heap)
    } yield {
      children match {
        case Nil => newHeap
        case _ => merge(children.reverse, newHeap)
      }
    }
}

object ExplicitMin {
  case class ExplicitMinHeap[+T](min: Option[T], heap: Heap[T]) extends Heap[T] {
    def findMin: Option[T] = min
    def deleteMin: Option[ExplicitMinHeap[T]] = min match {
      case None => None
      case Some(min) => for {
          newHeap <- heap.deleteMin
        } yield {
        val newMin = newHeap.findMin
        ExplicitMinHeap(newMin, newHeap)
      }
    }

    def merge[S >: T](other: Heap[S]): ExplicitMinHeap[S] = ExplicitMinHeap(List(this.min, other.min).min, this.heap.merge(other.heap))

    def insert[S >: T](element: S)(implicit cmp: S => Ordered[S]): ExplicitMinHeap[S] = {
      val newMin = this.min match {
        case None => element
        case Some(minValue) => if (element < minValue) element else minValue
      }
      ExplicitMinHeap(Some(newMin), this.heap.insert(element))
    }
  }
}