package heap

/**
 *
 */
object BinomialHeap extends HeapBuilder {

  sealed class BinomialTree[+A] {
    def link[B >: A](otherTree: BinomialTree[B])(implicit cmp: B => Ordered[B]): Option[BinomialTree[B]] =
      (this, otherTree) match {
        case (Node(e1, r1, children1), Node(e2, r2, children2)) =>
          if (e1 < e2 && r1 == r2) Some(Node(e1, r1 + 1, BinomialHeap(otherTree :: children1.trees)))
          else if (e1 >= e2 && r1 == r2) Some(Node(e2, r2 + 1, BinomialHeap(this :: children2.trees)))
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

  override def singletonHeap[A](element: A)(implicit cmp: A => Ordered[A]): BinomialHeap[A] =
    BinomialHeap(List(Node(element, 1, BinomialHeap(Nil)(cmp))))(cmp)
}
