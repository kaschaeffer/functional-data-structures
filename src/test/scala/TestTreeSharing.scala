import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Treeset.complete
import Treeset.Empty
import Treeset.Tree

/**
 * Created by schaeffer on 12/28/14.
 */

@RunWith(classOf[JUnitRunner])
class TestTreeSharing extends FunSuite {
  test("complete creates a complete tree") {
    val tree = complete(9, 3)
    val expectedTree = Tree(9,
      Tree(9, Tree(9, Empty, Empty), Tree(9, Empty, Empty)),
      Tree(9, Tree(9, Empty, Empty), Tree(9, Empty, Empty))
    )

    assert(tree === expectedTree)
  }
}
