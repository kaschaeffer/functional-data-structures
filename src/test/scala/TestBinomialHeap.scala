import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 12/29/14.
 */
@RunWith(classOf[JUnitRunner])
class TestBinomialHeap extends FunSuite {
  test("findMin for empty heap") {
    assert(BinomialHeap.BinomialHeap(Nil).findMin === None)
  }
}

