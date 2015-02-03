import heap.HeapBuilder
import org.scalatest.FunSuite

/**
 * Created by kevinschaeffer on 1/13/15.
 */
abstract class TestHeap extends FunSuite {
    def heapBuilder: HeapBuilder = ???

    test("findMin for non-empty heap") {
      val heap = heapBuilder.fromList(List(2, 10, 12, 8)).get
      assert(heap.findMin === Some(2))
    }

    test("deleteMin for non-empty heap") {
      val heap = heapBuilder.fromList(List(2, 10, 12, 8)).get

      val heapDeleted1 = heap.deleteMin.get
      assert(heapDeleted1.findMin === Some(8))

      val heapDeleted2 = heapDeleted1.deleteMin.get
      assert(heapDeleted2.findMin === Some(10))

      val heapDeleted3 = heapDeleted2.deleteMin.get
      assert(heapDeleted3.findMin === Some(12))
    }

    test("merging two non-empty heaps") {
      val heap = heapBuilder.fromList(List(2, 10, 12, 8)).get
      val heap2 = heapBuilder.fromList(List(11, 1, 9)).get

      println(heap)
      println(heap2)

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

//    test("merge empty and non-empty heap") {
//      val heap = heapBuilder.fromList(List(2, 10, 12, 8))
//      // val heap2 = Empty
//
//      assert(heap.merge(heap2) === heap)
//      assert(heap2.merge(heap) === heap)
//    }

    test("fromList for non-empty list") {
      val elements = List(9, 100, -2, 1, 101, 102, -3, 80)
      val heap = heapBuilder.fromList(elements).get

      assert(heap.findMin.get === -3)
      assert(heap.deleteMin.get.findMin.get === -2)
      assert(heap.deleteMin.get.deleteMin.get.findMin.get === 1)
      assert(heap.deleteMin.get.deleteMin.get.deleteMin.get.findMin.get === 9)
    }
  }
