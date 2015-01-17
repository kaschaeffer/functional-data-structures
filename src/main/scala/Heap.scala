/**
 * Created by schaeffer on 12/29/14.
 */
sealed trait Heap[+A] {
  def empty: Boolean
  def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): Heap[B]
  def findMin: Option[A]
  def deleteMin: Option[Heap[A]]
  def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): Heap[B]
}

object BinaryHeap {
  class BinaryHeap[+A](implicit baseCmp: A => Ordered[A]) extends Heap[A] {
    def empty: Boolean = this match {
      case EmptyNode => true
      case _ => false
    }

    def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): BinaryHeap[B] = this match {
      case EmptyNode => Node(element, 1, EmptyNode, EmptyNode)
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
      case EmptyNode => None
      case Node(element, _, _, _) => Some(element)
    }

    private def size: Int = this match {
      case EmptyNode => 0
      case Node(e, size, left, right) => size
    }

    def deleteMin: Option[BinaryHeap[A]] = this match {
      case EmptyNode => None
      case Node(element, size, left, right) =>
        val (newElement, newHeap) = this.swap.get
        newHeap match {
          case EmptyNode => Some(EmptyNode)
          case Node(oldRoot, newSize, newLeft, newRight) => Some(
            Node(newElement, newSize, newLeft, newRight).sink.get)
        }
    }

    private def swap: Option[(A, BinaryHeap[A])] = this match {
      case EmptyNode => None
      //TODO above should be unreachable as we're using the function (refactor to raise exception)
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

    def sink: Option[BinaryHeap[A]] = this match {
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

    def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): BinaryHeap[B] = other match {
      case EmptyNode => {
        this
      }
      case Node(e, _, _, _) => other.deleteMin match {
        case None => {
          // TODO best way to indicate failure here?
          this
        } /*
          Note: this should never be executed since deleteMin cannot fail
          when the heap is NonEmpty */
        case Some(newOther) => {
          this.insert(e).merge(newOther)
        }
      }
    }
  }


  case object EmptyNode extends BinaryHeap[Nothing]
  case class Node[A](element: A, size: Int, left: BinaryHeap[A], right: BinaryHeap[A])
    (implicit cmp: A => Ordered[A]) extends BinaryHeap[A]

}

object LeftistHeap {
  class LeftistHeap[+A](implicit cmp: A => Ordered[A]) extends Heap[A] {
    def empty: Boolean = this match {
      case Empty => false
      case _ => true
    }

    def findMin: Option[A] = this match {
      case Empty => None
      case Node(element, _, _, _) => Some(element)
    }

//    def merge[B >: A](other: Heap[B])(implicit cmp: B => Ordered[B]): LeftistHeap[B] =
//      this match {
//        case Empty => other
//        case Node(element, rank, left, right) => other match {
//            case Empty => this
//            case Node(otherElement, otherRank, otherLeft, otherRight) =>
//              if (otherElement > element) makeNode(element, left, right.merge(other))
//              else makeNode(otherElement, otherLeft, otherRight.merge(this))
//          }
//        case _ => throw IllegalArgumentException
//    }

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

  def fromList[A](elements: List[A])(implicit cmp: A => Ordered[A]): LeftistHeap[A] =
    mergeList(elements map (x => Node(x, 1, Empty, Empty))) match {
      case Nil => Empty
      case x::_ => x
  }

  private def mergeList[A](heaps: List[LeftistHeap[A]])(implicit cmp: A => Ordered[A]): List[LeftistHeap[A]] =
    heaps match {
      case Nil => Nil
      case x::Nil => List(x)
      case x::y::xs => mergeList(x.merge(y)::mergeList(xs))
  }

  case object Empty extends LeftistHeap[Nothing]
  case class Node[A](element: A, private val _rank: Int, left: LeftistHeap[A], right: LeftistHeap[A])
    (implicit cmp: A => Ordered[A]) extends LeftistHeap[A]
}

object WeightLeftistHeap {

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

  def fromList[A](elements: List[A])(implicit cmp: A => Ordered[A]): WeightLeftistHeap[A] =
    mergeList(elements map (x => Node(x, 1, Empty, Empty))) match {
      case Nil => Empty
      case x::_ => x
    }

  private def mergeList[A](heaps: List[WeightLeftistHeap[A]])(implicit cmp: A => Ordered[A]): List[WeightLeftistHeap[A]] =
    heaps match {
      case Nil => Nil
      case x::Nil => List(x)
      case x::y::xs => mergeList(x.merge(y)::mergeList(xs))
    }
}

object BinomialHeap {

  sealed class BinomialTree[+A] {
    def link[B >: A](otherTree: BinomialTree[B])(implicit cmp: B => Ordered[B]): Option[BinomialTree[B]] =
      (this, otherTree) match {
        case (Node(e1, r1, children1), Node(e2, r2, children2)) =>
          if (e1 < e2 && r1 == r2) Some(Node(e1, r1 + 1, BinomialHeap(otherTree :: children1.trees)))
          else if (e1 <= e2 && r1 == r2) Some(Node(e2, r2 + 1, BinomialHeap(this :: children2.trees)))
          else None
      }
  }

  case class Node[+A](element: A, rank: Integer, children: BinomialHeap[A]) extends BinomialTree[A]


  case class BinomialHeap[+A](trees: List[BinomialTree[A]])(implicit baseCmp: A => Ordered[A]) extends Heap[A] {
    def empty: Boolean = this match {
      case BinomialHeap(Nil) => false
      case _ => true
    }

    def merge[B >: A](otherHeap: Heap[B])(implicit cmp: B => Ordered[B]): BinomialHeap[B] = (this, otherHeap) match {
      case (BinomialHeap(Nil), BinomialHeap(trees)) => BinomialHeap(trees)
      case (_, BinomialHeap(Nil)) => this
      case (BinomialHeap((tree1@Node(e1, r1, children1))::rest1), BinomialHeap((tree2@Node(e2, r2, children2))::rest2)) =>
        if (r2 > r1) BinomialHeap(List(tree1))
        else if (r2 < r1) BinomialHeap(List(tree2))
        else BinomialHeap(List(tree1.link(tree2).get))
          .merge(BinomialHeap(rest1))
          .merge(BinomialHeap(rest2))
      case _ => this // TODO indicate failure properly
    }

    def insert[B >: A](element: B)(implicit cmp: B => Ordered[B]): BinomialHeap[B] =
      this.merge(BinomialHeap(List(Node(element, 1, BinomialHeap(Nil)(cmp))))(cmp))

    def findMin: Option[A] = for {
      (Node(element, _, _), _) <- this.removeMinTree
    } yield element

    def removeMinTree: Option[(BinomialTree[A], BinomialHeap[A])] = this.trees match {
      case Nil => None
      case List(x) => Some((x, BinomialHeap(Nil)(baseCmp)))
      case (x@Node(e, _, _)) :: xs => for {
        (xsTree@Node(exs, _, _), xsRest) <- BinomialHeap(xs).removeMinTree
      } yield {
        if (e < exs) (x, BinomialHeap(xs))
        else (xsTree, xsRest)
      }
    }

    def deleteMin: Option[BinomialHeap[A]] = for {
      (Node(e, _, children), newHeap) <- this.removeMinTree
    } yield {
      children.trees match {
        case Nil => newHeap
        case _ => BinomialHeap(children.trees.reverse).merge(newHeap)
      }
    }
  }
}

object ExplicitMin {

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