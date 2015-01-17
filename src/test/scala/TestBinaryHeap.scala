import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import BinaryHeap.EmptyNode
import BinaryHeap.Node

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

  test("merging two non-empty heaps") {
    val heap = Node(2, 4,
      Node(10, 2, Node(12, 1, EmptyNode, EmptyNode), EmptyNode),
      Node(8, 1, EmptyNode, EmptyNode))

    val heap2 = Node(1, 3,
      Node(9, 1, EmptyNode, EmptyNode),
      Node(11, 1, EmptyNode, EmptyNode))

    val mergedHeap = heap.merge(heap2)
    val mergedHeap2 = heap2.merge(heap)

    assert(mergedHeap.findMin.get === 1)
    assert(mergedHeap.
      deleteMin.get.findMin.get === 2)
    assert(mergedHeap.
      deleteMin.get.
      deleteMin.get.findMin.get === 8)
    assert(mergedHeap.
      deleteMin.get.
      deleteMin.get.
      deleteMin.get.findMin.get === 9)

    assert(mergedHeap2.findMin.get === 1)
    assert(mergedHeap2.
      deleteMin.get.findMin.get === 2)
    assert(mergedHeap2.
      deleteMin.get.
      deleteMin.get.findMin.get === 8)
    assert(mergedHeap2.
      deleteMin.get.
      deleteMin.get.
      deleteMin.get.findMin.get === 9)
  }

  test("merge empty and non-empty heap") {
    val heap = Node(2, 4,
      Node(10, 2, Node(12, 1, EmptyNode, EmptyNode), EmptyNode),
      Node(8, 1, EmptyNode, EmptyNode))

    val heap2 = EmptyNode

    assert(heap.merge(heap2) === heap)
  }
}
