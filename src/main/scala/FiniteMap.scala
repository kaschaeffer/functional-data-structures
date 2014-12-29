/**
 * Created by schaeffer on 12/29/14.
 */
sealed trait FiniteMap[+K, +V] {
  def empty: Boolean
  def insert[J >: K, U >: V](key: J, value: U)(implicit cmp: J => Ordered[J]): FiniteMap[J, U]
  def lookup[J >: K](key: J)(implicit cmp: J => Ordered[J]): Option[V]
}

sealed trait FiniteMapTree[+K, +V] extends FiniteMap[K, V] {
  def empty: Boolean = this match {
    case MapEmpty  => true
    case _      => false
  }

  def insert[J >: K, U >: V](key: J, value: U)(implicit cmp: J => Ordered[J]): FiniteMapTree[J, U] = this match {
    case MapEmpty => MapTree(key, value, MapEmpty, MapEmpty)
    case MapTree(k, v, left, right) =>
      if (key > k) MapTree(k, v, left, right.insert(key, value))
      else if (key < k) MapTree(k, v, left.insert(key, value), right)
      else MapTree(key, value, MapEmpty, MapEmpty)
  }

  def lookup[J >: K](key: J)(implicit cmp: J => Ordered[J]): Option[V] = this match {
    case MapEmpty => None
    case MapTree(k, v, left, right) =>
      if (key > k) right.lookup(key)
      else if (key < k) left.lookup(key)
      else Some(v)
  }
}

case object MapEmpty extends FiniteMapTree[Nothing, Nothing]
case class MapTree[K, V](key: K, value: V, left: FiniteMapTree[K, V], right: FiniteMapTree[K, V])
  extends FiniteMapTree[K, V]
