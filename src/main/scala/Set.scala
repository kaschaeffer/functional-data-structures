/**
 * Created by schaeffer on 12/28/14.
 */
sealed trait Set[+A] {
  def empty: Boolean
  def insert[B >: A](elem: B)(implicit cmp: B => Ordered[B]): Set[B]
  def member[B >: A](elem: B)(implicit cmp: B => Ordered[B]): Boolean
}

object Treeset {
  sealed trait Treeset[+A] extends Set[A] {
    override def empty: Boolean = this match {
      case Empty => true
      case _ => false
    }

    override def insert[B >: A](elem: B)(implicit cmp: B => Ordered[B]): Treeset[B] =
      efficientInsert(elem, None) match {
        case None => this
        case Some(tree) => tree
      }

    private def efficientInsert[B >: A](elem: B, candidateElem: Option[B])
      (implicit cmp: B => Ordered[B]): Option[Treeset[B]] =
      this match {
        case Empty => candidateElem match {
          case None => Some(Tree(elem, Empty, Empty))
          case Some(candidate) =>
            if (elem == candidate) None
            else Some(Tree(elem, Empty, Empty))
        }
        case Tree(e, left, right) =>
          if (elem > e) right.efficientInsert(elem, candidateElem) map { newRight => Tree(e, left, newRight)}
          else left.efficientInsert(elem, Some(e)) map { newLeft => Tree(e, newLeft, right)}
      }

    override def member[B >: A](elem: B)(implicit cmp: B => Ordered[B]): Boolean =
      fastMember(elem, None)

    private def fastMember[B >: A](elem: B, candidateElem: Option[B])
      (implicit cmp: B => Ordered[B]): Boolean = this match {
      case Empty => candidateElem match {
        case None => false
        case Some(candidate) => elem == candidate
      }
      case Tree(e, left, right) =>
        if (elem > e) right.fastMember(elem, candidateElem)
        else left.fastMember(elem, Some(e))
    }
  }

  case object Empty extends Treeset[Nothing]
  case class Tree[A](elem: A, left: Treeset[A], right: Treeset[A]) extends Treeset[A]


  // Generates a complete tree, consisting only of elem
  def complete[A](elem: A, depth: Int): Treeset[A] = depth match {
    case 1 => Tree(elem, Empty, Empty)
    case d => {
      val subtree = complete(elem, d - 1)
      Tree(elem, subtree, subtree)
    }
  }

  // Generates a maximally balanced tree of arbitrary size, consisting
  // only of elem
  def balanced[A](elem: A, size: Int): Treeset[A] = {
    if (size % 2 == 1) {
      val (subtree, _) = create2(elem, (size-1)/2)
      Tree(elem, subtree, subtree)
    } else {
      val (smallSubtree, largeSubtree) = create2(elem, (size/2) - 1)
      Tree(elem, smallSubtree, largeSubtree)
    }
  }

  // Creates a pair of trees with sizes (size, size + 1)
  def create2[A](elem: A, size: Int): (Treeset[A], Treeset[A]) = size match {
    case 0 => (Empty, Tree(elem, Empty, Empty))
    case n => {
      val subtreeSize = (n - 1)/2
      val (small, large) = create2(elem, subtreeSize)
      (Tree(elem, small, small), Tree(elem, small, large))
    }
  }

  def size[A](tree: Treeset[A]): Int = tree match {
    case Empty => 0
    case Tree(_, left, right) => 1 + size(left) + size(right)
  }
}

