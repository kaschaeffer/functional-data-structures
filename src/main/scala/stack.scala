package fds
/**
 * Created by schaeffer on 12/27/14.
 */
sealed trait Stack[+T] {
  def isEmpty: Boolean
  def head: Option[T]
  def tail: Option[Stack[T]]
  def cons[S >: T](elem: S): Stack[S]
  def append[S >: T](other: Stack[S]): Stack[S]
  def suffixes: Stack[Stack[T]]
}

object CustomStack {
  sealed trait CustomStack[+T] extends Stack[T] {
    override def isEmpty: Boolean = this match {
      case Nil => true
      case _ => false
    }

    override def head: Option[T] = this match {
      case Nil => None
      case Cons(elem, _) => Some(elem)
    }

    override def tail: Option[Stack[T]] = this match {
      case Nil => None
      case Cons(_, rest) => Some(rest)
    }

    override def cons[S >: T](elem: S): Stack[S] = Cons(elem, this)

    override def append[S >: T](other: Stack[S]): Stack[S] = this match {
      case Nil => other
      case Cons(elem, Nil) => Cons(elem, other)
      case Cons(elem, rest) => Cons(elem, rest.append(other))
    }

    override def suffixes: Stack[Stack[T]] = this match {
      case Nil => Nil
      case Cons(elem, Nil) => Cons(this, Nil)
      case Cons(elem, rest) => Cons(this, rest.suffixes)
    }
  }

  case object Nil extends CustomStack[Nothing]
  case class Cons[+T](elem: T, rest: Stack[T]) extends CustomStack[T]
}