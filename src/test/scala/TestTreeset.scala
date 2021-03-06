import org.junit.runner.RunWith
import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.const
import org.scalacheck.Gen.oneOf

import Treeset.size
import Treeset.Empty
import Treeset.Tree
import Treeset.Treeset

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

  test("insert existing value into large set") {
    val tree = Tree(10, Tree(3, Empty, Tree(8, Tree(4, Empty, Tree(6, Empty, Empty)), Empty)), Tree(20, Empty, Empty))
    assert(tree.insert(6) === tree)
    assert(tree.insert(8) === tree)
    assert(tree.insert(20) === tree)
  }

  test("insert new value into large set") {
    val tree = Tree(10, Tree(3, Empty, Tree(8, Tree(4, Empty, Tree(6, Empty, Empty)), Empty)), Tree(20, Empty, Empty))
    val newTree = Tree(10, Tree(3, Empty, Tree(8, Tree(4, Empty, Tree(6, Tree(5, Empty, Empty), Empty)), Empty)), Tree(20, Empty, Empty))
    assert(tree.insert(5) === newTree)
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

  test("member in deep tree") {
    val tree = Tree(10, Tree(3, Empty, Tree(8, Tree(4, Empty, Tree(6, Empty, Empty)), Empty)), Empty)
    assert(tree.member(8) === true)
    assert(tree.member(6) === true)
    assert(tree.member(4) === true)
    assert(tree.member(5) === false)
    assert(tree.member(3) === true)
    assert(tree.member(10) === true)
    assert(tree.member(2) === false)
  }

  test("member when element is not a member") {
    val tree = Tree(10, Tree(3, Empty, Empty), Tree(25, Empty, Empty))
    assert(tree.member(29) === false)
  }
}

object TreesetSpecification extends Properties("Treeset") {
  val genTree = for {
    e <- arbitrary[Int]
    left <- genTreeset
    right <- genTreeset
  } yield Tree(e, left, right)
  val genEmpty = const(Empty)
  def genTreeset: Gen[Treeset[Int]] = oneOf(genTree, genEmpty)
  implicit val arbitraryTreeset = Arbitrary(genTreeset)

  property("calling member after insert returns true") = forAll { (a: Treeset[Int], b: Int) =>
    a.insert(b).member(b)
  }

  property("insert is idempotent") = forAll { (a: Treeset[Int], b: Int) =>
    a.insert(b) == a.insert(b).insert(b)
  }

  property("insert does not decrease set size") = forAll { (a: Treeset[Int], b: Int) =>
    size(a.insert(b)) >= size(a)
  }
}
