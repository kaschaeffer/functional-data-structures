import heap.{LeftistHeap, HeapBuilder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 1/2/15.
 */

@RunWith(classOf[JUnitRunner])
class TestLeftistHeap extends TestHeap {
  override def heapBuilder: HeapBuilder = LeftistHeap
}
