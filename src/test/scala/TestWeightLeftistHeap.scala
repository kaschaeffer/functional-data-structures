import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import WeightLeftistHeap.Empty
import WeightLeftistHeap.fromList
import WeightLeftistHeap.Node

/**
 * Created by schaeffer on 1/2/15.
 */

@RunWith(classOf[JUnitRunner])
class TestWeightLeftistHeap extends FunSuite {
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

  test("merging two non-empty heaps") {
    val heap = Node(2, 2,
      Node(10, 1, Node(12, 1, Empty, Empty), Empty),
      Node(8, 1, Empty, Empty))

    val heap2 = Node(1, 2,
      Node(9, 1, Empty, Empty),
      Node(11, 1, Empty, Empty))

    val mergedHeap = heap.merge(heap2)
    val mergedHeap2 = heap2.merge(heap)

    assert(mergedHeap.findMin.get === 1)
    assert(mergedHeap.deleteMin.get.findMin.get === 2)
    assert(mergedHeap.deleteMin.get.deleteMin.get.findMin.get === 8)
    assert(mergedHeap.deleteMin.get.deleteMin.get.deleteMin.get.findMin.get === 9)

    assert(mergedHeap2.findMin.get === 1)
    assert(mergedHeap2.deleteMin.get.findMin.get === 2)
    assert(mergedHeap2.deleteMin.get.deleteMin.get.findMin.get === 8)
    assert(mergedHeap2.deleteMin.get.deleteMin.get.deleteMin.get.findMin.get === 9)
  }

  test("merge empty and non-empty heap") {
    val heap = Node(2, 2,
      Node(10, 1, Node(12, 1, Empty, Empty), Empty),
      Node(8, 1, Empty, Empty))

    val heap2 = Empty

    assert(heap.merge(heap2) === heap)
    assert(heap2.merge(heap) === heap)
  }

  test("fromList for non-empty list") {
    val elements = List(9, 100, -2, 1, 101, 102, -3, 80)
    val heap = fromList(elements)

    assert(heap.findMin.get === -3)
    assert(heap.deleteMin.get.findMin.get === -2)
    assert(heap.deleteMin.get.deleteMin.get.findMin.get === 1)
    assert(heap.deleteMin.get.deleteMin.get.deleteMin.get.findMin.get === 9)
  }
}

