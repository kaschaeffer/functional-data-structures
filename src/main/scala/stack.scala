package fds
/**
 * Created by schaeffer on 12/27/14.
 */
sealed trait Stack[+A] {
  def isEmpty: Boolean
  def head: Option[A]
  def tail: Option[Stack[A]]
  def cons[B >: A](elem: B): Stack[B]
  def append[B >: A](other: Stack[B]): Stack[B]
  def suffixes: Stack[Stack[A]]
}

object CustomStack {
  sealed trait CustomStack[+A] extends Stack[A] {
    override def isEmpty: Boolean = this match {
      case Nil => true
      case _ => false
    }

    override def head: Option[A] = this match {
      case Nil => None
      case Cons(elem, _) => Some(elem)
    }

    override def tail: Option[Stack[A]] = this match {
      case Nil => None
      case Cons(_, rest) => Some(rest)
    }

    override def cons[B >: A](elem: B): Stack[B] = Cons(elem, this)

    override def append[B >: A](other: Stack[B]): Stack[B] = this match {
      case Nil => other
      case Cons(elem, Nil) => Cons(elem, other)
      case Cons(elem, rest) => Cons(elem, rest.append(other))
    }

    override def suffixes: Stack[Stack[A]] = this match {
      case Nil => Nil
      case Cons(elem, Nil) => Cons(this, Nil)
      case Cons(elem, rest) => Cons(this, rest.suffixes)
    }
  }

  case object Nil extends CustomStack[Nothing]
  case class Cons[+A](elem: A, rest: Stack[A]) extends CustomStack[A]
}