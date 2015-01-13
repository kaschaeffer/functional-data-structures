package trees

object RedBlackTree {
  sealed trait Color
  case object Red extends Color
  case object Black extends Color

  // TODO: better understand covariance!
  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[A](color: Color, elem: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  //def member[T](tree: Tree[T], elem: T)(implicit cmp : T => Ordered[T]): Boolean = tree match {
  //  case Empty => false
  //  case Node(_, that, left, right) if elem == that => true
  //  case Node(_, that, left, right) if (elem > that) => member(right, elem)
  //  case Node(_, that, left, right) if elem < that => member(left, elem)
  //}

  def member[A](tree: Tree[A], elem: A)(implicit cmp : A => Ordered[A]): Boolean = tree match {
    case Empty => false
    case Node(_, that, left, right) =>
      if (elem == that) true
      else if (elem > that) member(right, elem)
      else member(left, elem)
  }

  def insert[A](tree: Tree[A], elem: A): Tree[A] = tree match {
    case Empty => Node(Black, elem, Empty, Empty)
    case _ => Empty
  }

  //def insert[T](tree: Tree, elem: T): Tree[T] = tree match {
  //
  //}
}