package fds
/**
 * Created by schaeffer on 12/27/14.
 */
object Stack {

  sealed abstract class Stack[+T]
  case object Nil extends Stack[Nothing]
  case class Cons[T](elem: T, rest: Stack[T]) extends Stack[T]

  def isEmpty[T](stack: Stack[T]): Boolean = stack match {
    case Nil => true
    case _ => false
  }

  def head[T](stack: Stack[T]): Option[T] = stack match {
    case Nil => None
    case Cons(elem, _) => Some(elem)
  }

  def tail[T](stack: Stack[T]): Option[Stack[T]] = stack match {
    case Nil => None
    case Cons(_, rest) => Some(rest)
  }

  def cons[T](stack: Stack[T], elem: T): Stack[T] = Cons(elem, stack)

  def append[T](stack: Stack[T], other: Stack[T]): Stack[T] = stack match {
    case Nil => other
    case Cons(elem, Nil) => Cons(elem, other)
    case Cons(elem, rest) => Cons(elem, append(rest, other))
  }

  def suffixes[T](stack: Stack[T]): Stack[Stack[T]] = stack match {
    case Nil => Nil
    case Cons(elem, Nil) => Cons(stack, Nil)
    case Cons(elem, rest) => Cons(stack, suffixes(rest))
  }
}