import heap.{WeightLeftistHeap, HeapBuilder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 1/2/15.
 */

@RunWith(classOf[JUnitRunner])
class TestWeightLeftistHeap extends TestHeap {
  override def heapBuilder: HeapBuilder = WeightLeftistHeap
}

