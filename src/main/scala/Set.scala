/**
 * Created by schaeffer on 12/28/14.
 */
sealed trait Set[+T] {
  def empty: Boolean
  def insert[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Set[S]
  def member[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Boolean
}

sealed trait Treeset[+T] extends Set[T] {
  override def empty: Boolean = this match {
    case Empty  => true
    case _      => false
  }

  override def insert[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Treeset[S] =
    efficientInsert(elem, None) match {
      case None       => this
      case Some(tree) => tree
    }

  // TODO clean this up by rewriting using monads
  private def efficientInsert[S >: T](elem: S, candidateElem: Option[S])(implicit cmp: S => Ordered[S]): Option[Treeset[S]] =
    this match {
      case Empty => candidateElem match {
        case None => Some(Tree(elem, Empty, Empty))
        case Some(candidate) =>
          if (elem == candidate) None
          else Some(Tree(elem, Empty, Empty))
      }
      case Tree(e, left, right) =>
        if (elem > e) {
          val newRight = right.efficientInsert(elem, candidateElem)
          newRight match {
            case None => None
            case Some(r) => Some(Tree(e, left, r))
          }
        }
        else {
          val newLeft = left.efficientInsert(elem, Some(e))
          newLeft match {
            case None => None
            case Some(l) => Some(Tree(e, l, right))
          }
        }
    }

  override def member[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Boolean =
    fastMember(elem, None)

  private def fastMember[S >: T](elem: S, candidateElem: Option[S])(implicit cmp: S => Ordered[S]): Boolean = this match {
    case Empty => candidateElem match {
      case None             => false
      case Some(candidate)  => elem == candidate
    }
    case Tree(e, left, right) =>
      if (elem > e) right.fastMember(elem, candidateElem)
      else left.fastMember(elem, Some(e))
  }
}

case object Empty extends Treeset[Nothing]
case class Tree[T](elem: T, left: Treeset[T], right: Treeset[T]) extends Treeset[T]
