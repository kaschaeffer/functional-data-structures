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

  override def insert[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Set[S] = this match {
    case Empty => Tree(elem, Empty, Empty)
    case Tree(e, left, right) =>
      if (elem == e) this
      else if (elem > e) Tree(e, left, right.insert(elem))
      else Tree(e, left.insert(elem), right)
  }

  override def member[S >: T](elem: S)(implicit cmp: S => Ordered[S]): Boolean = this match {
    case Empty => false
    case Tree(e, left, right) =>
      if (elem == e) true
      else if (elem > e) right.member(elem)
      else left.member(elem)
  }
}

case object Empty extends Treeset[Nothing]
case class Tree[T](elem: T, left: Set[T], right: Set[T]) extends Treeset[T]
