import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 12/28/14.
 */

@RunWith(classOf[JUnitRunner])
class TestTreeset extends FunSuite {
  test("empty when empty") {
    assert(Empty.empty === true)
  }

  test("empty when nonempty") {
    assert(Tree(3, Empty, Empty).empty === false)
  }

  test("insert into empty") {
    val treeset = Empty
    assert(treeset.insert(10) === Tree(10, Empty, Empty))
  }

  test("insert new value into nonempty set") {
    val tree = Tree(10, Tree(3, Empty, Empty), Tree(25, Empty, Empty))
    val newTree = tree.insert(21)
    assert(newTree === Tree(10, Tree(3, Empty, Empty), Tree(25, Tree(21, Empty, Empty), Empty)))
  }

  test("insert existing value into nonempty set") {
    val tree = Tree(10, Tree(3, Empty, Empty), Tree(25, Empty, Empty))
    assert(tree.insert(3) === tree)
  }

  test("member for empty set") {
    assert(Empty.member(9) === false)
  }

  test("member when is a member at root") {
    val tree = Tree(10, Tree(3, Empty, Empty), Tree(25, Empty, Empty))
    assert(tree.member(10) === true)
  }

  test("member when is a member at leaf") {
    val tree = Tree(10, Tree(3, Empty, Empty), Tree(25, Empty, Empty))
    assert(tree.member(25) === true)
  }

  test("member when element is not a member") {
    val tree = Tree(10, Tree(3, Empty, Empty), Tree(25, Empty, Empty))
    assert(tree.member(29) === false)
  }
}
