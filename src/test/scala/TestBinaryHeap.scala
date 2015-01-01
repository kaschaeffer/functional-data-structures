import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 12/29/14.
 */
@RunWith(classOf[JUnitRunner])
class TestBinaryHeap extends FunSuite {
  test("findMin for empty heap") {
    assert(EmptyNode.findMin === None)
  }

  test("findMin for non-empty heap") {
    val heap = Node(2, 4,
      Node(10, 2, Node(12, 1, EmptyNode, EmptyNode), EmptyNode),
      Node(8, 1, EmptyNode, EmptyNode))
    assert(heap.findMin === Some(2))
  }

  test("deleteMin for non-empty heap") {
    val heap = Node(2, 4,
      Node(10, 2, Node(12, 1, EmptyNode, EmptyNode), EmptyNode),
      Node(8, 1, EmptyNode, EmptyNode))

    val heapDeleted1 = heap.deleteMin.get
    assert(heapDeleted1.findMin === Some(8))

    val heapDeleted2 = heapDeleted1.deleteMin.get
    assert(heapDeleted2.findMin === Some(10))

    val heapDeleted3 = heapDeleted2.deleteMin.get
    assert(heapDeleted3.findMin === Some(12))
  }
}
