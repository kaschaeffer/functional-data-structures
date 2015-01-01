/**
 * Created by schaeffer on 12/29/14.
 */
sealed trait Heap[+T] {
  def empty: Boolean
  def insert[S >: T](element: S)(implicit cmp: S => Ordered[S]): Heap[S]
  def findMin: Option[T]
  def deleteMin: Option[Heap[T]]
  // TODO: merge
  // def merge[S >: T](other: S): Heap[S]
}

class BinaryHeap[+T](implicit cmp: T => Ordered[T]) extends Heap[T] {
  def empty: Boolean = this match {
    case EmptyNode  => true
    case _          => false
  }

  def insert[S >: T](element: S)(implicit cmp: S => Ordered[S]): Heap[S] = this match {
    case EmptyNode => Node(element, 1, EmptyNode, EmptyNode)
    case Node(e, size, left, right) =>
      if (left.size < right.size) {
        val Node(newElement, rightSize, rightLeft, rightRight) = right.insert(element)
        if (newElement < e) Node(newElement, size + 1, left, Node(e, rightSize, rightLeft, rightRight))
        else Node(e, size, left, Node(newElement, rightSize, rightLeft, rightRight))
      } else {
        val Node(newElement, leftSize, leftLeft, leftRight) = left.insert(element)
        if (newElement < e) Node(newElement, size + 1, Node(e, leftSize, leftLeft, leftRight), right)
        else Node(e, size, Node(newElement, leftSize, leftLeft, leftRight), right)
      }
  }

  def findMin: Option[T] = this match {
    case EmptyNode              => None
    case Node(element, _, _, _) => Some(element)
  }

  private def size: Int = this match {
    case EmptyNode                  => 0
    case Node(e, size, left, right) => size
  }

  def deleteMin: Option[BinaryHeap[T]] = this match {
    case EmptyNode => None
    case Node(element, size, left, right) => {
      val (newElement, newHeap) = this.swap.get
      newHeap match {
        case EmptyNode => Some(EmptyNode)
        case Node(oldRoot, newSize, newLeft, newRight) => Some(Node(newElement, newSize, newLeft, newRight).sink.get)
      }
    }
  }

  private def swap: Option[(T, BinaryHeap[T])] = this match {
    case EmptyNode => None
    // TODO above should be unreachable as we're using the function (refactor to raise exception)
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

  def sink: Option[BinaryHeap[T]] = this match {
    case EmptyNode => None
    case Node(e, size, EmptyNode, EmptyNode) => Some(this)
    case Node(e, size, left, EmptyNode) =>
      val Node(leftE, leftSize, leftLeft, leftRight) = left
      if (leftE < e) Some(Node(leftE, size, Node(e, leftSize, leftLeft, leftRight), EmptyNode))
      else Some(this)
    case Node(e, size, EmptyNode, right@Node(rightE, rightSize, rightLeft, rightRight)) =>
      if (e > rightE) Some(Node(rightE, size, EmptyNode, Node(e, rightSize, rightLeft, rightRight)))
      else Some(this)
    case Node(e, size, left@Node(leftE, leftSize, leftLeft, leftRight), right@Node(rightE, rightSize, rightLeft, rightRight)) =>
      if (e > rightE && rightE > leftE) Some(Node(leftE, size, Node(e, leftSize, leftLeft, leftRight).sink.get, right))
      else if (e > leftE && leftE > rightE) Some(Node(rightE, size, left, Node(e, rightSize, rightLeft, rightRight).sink.get))
      else if (leftE > e && e > rightE) Some(Node(leftE, size, Node(e, leftSize, leftLeft, leftRight).sink.get, right))
      else if (rightE > e && e > leftE) Some(Node(rightE, size, left, Node(e, rightSize, rightLeft, rightRight).sink.get))
      else Some(this)
  }
}

case object EmptyNode extends BinaryHeap[Nothing]
case class Node[T](element: T, size: Int, left: BinaryHeap[T], right: BinaryHeap[T])(implicit cmp: T => Ordered[T]) extends BinaryHeap[T]
