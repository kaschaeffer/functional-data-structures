import heap.{BinaryHeap, HeapBuilder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 12/29/14.
 */
@RunWith(classOf[JUnitRunner])
class TestBinaryHeap extends TestHeap {
  override def heapBuilder: HeapBuilder = BinaryHeap
}
