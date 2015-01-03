/**
 * Created by schaeffer on 12/28/14.
 */
sealed trait Set[+T] {
  def empty: Boolean
  def insert[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Set[S]
  def member[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Boolean
}

object Treeset {
  sealed trait Treeset[+T] extends Set[T] {
    override def empty: Boolean = this match {
      case Empty => true
      case _ => false
    }

    override def insert[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Treeset[S] =
      efficientInsert(elem, None) match {
        case None => this
        case Some(tree) => tree
      }

    private def efficientInsert[S >: T](elem: S, candidateElem: Option[S])
      (implicit cmp: S => Ordered[S]): Option[Treeset[S]] =
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

    override def member[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Boolean =
      fastMember(elem, None)

    private def fastMember[S >: T](elem: S, candidateElem: Option[S])
      (implicit cmp: S => Ordered[S]): Boolean = this match {
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
  case class Tree[T](elem: T, left: Treeset[T], right: Treeset[T]) extends Treeset[T]


  // Generates a complete tree, consisting only of elem
  def complete[T](elem: T, depth: Int): Treeset[T] = depth match {
    case 1 => Tree(elem, Empty, Empty)
    case d => {
      val subtree = complete(elem, d - 1)
      Tree(elem, subtree, subtree)
    }
  }

  // Generates a maximally balanced tree of arbitrary size, consisting
  // only of elem
  def balanced[T](elem: T, size: Int): Treeset[T] = {
    if (size % 2 == 1) {
      val (subtree, _) = create2(elem, (size-1)/2)
      Tree(elem, subtree, subtree)
    } else {
      val (smallSubtree, largeSubtree) = create2(elem, (size/2) - 1)
      Tree(elem, smallSubtree, largeSubtree)
    }
  }

  // Creates a pair of trees with sizes (size, size + 1)
  def create2[T](elem: T, size: Int): (Treeset[T], Treeset[T]) = size match {
    case 0 => (Empty, Tree(elem, Empty, Empty))
    case n => {
      val subtreeSize = (n - 1)/2
      val (small, large) = create2(elem, subtreeSize)
      (Tree(elem, small, small), Tree(elem, small, large))
    }
  }

  def size[T](tree: Treeset[T]): Int = tree match {
    case Empty => 0
    case Tree(_, left, right) => 1 + size(left) + size(right)
  }
}

