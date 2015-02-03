import heap.{BinomialHeap, HeapBuilder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 12/29/14.
 */
@RunWith(classOf[JUnitRunner])
class TestBinomialHeap extends TestHeap {
  override def heapBuilder: HeapBuilder = BinomialHeap
}


