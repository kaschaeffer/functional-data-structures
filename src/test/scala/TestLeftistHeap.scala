import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import LeftistHeap.Empty
import LeftistHeap.Node

/**
 * Created by schaeffer on 1/2/15.
 */

@RunWith(classOf[JUnitRunner])
class TestLeftistHeap extends FunSuite {
  test("findMin for empty heap") {
    assert(Empty.findMin === None)
  }

  test("findMin for non-empty heap") {
    val heap = Node(2, 2,
      Node(10, 1, Node(12, 1, Empty, Empty), Empty),
      Node(8, 1, Empty, Empty))
    assert(heap.findMin === Some(2))
  }

  test("deleteMin for non-empty heap") {
    val heap = Node(2, 2,
      Node(10, 1, Node(12, 1, Empty, Empty), Empty),
      Node(8, 1, Empty, Empty))

    val heapDeleted1 = heap.deleteMin.get
    assert(heapDeleted1.findMin === Some(8))

    val heapDeleted2 = heapDeleted1.deleteMin.get
    assert(heapDeleted2.findMin === Some(10))

    val heapDeleted3 = heapDeleted2.deleteMin.get
    assert(heapDeleted3.findMin === Some(12))
  }
}
