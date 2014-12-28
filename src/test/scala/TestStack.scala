package fds

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner



/**
 * Created by schaeffer on 12/27/14.
 */
@RunWith(classOf[JUnitRunner])
class TestStack extends FunSuite {
  test("isEmpty when empty") {
    assert(Nil.isEmpty === true)
  }

  test("isEmpty when nonempty") {
    assert(Cons(1, Nil).isEmpty === false)
  }

  test("head when empty") {
    val stack = Nil
    assert(stack.head === None)
  }

  test("head when nonempty") {
    val stack = Cons(9, Cons(7, Cons(1, Nil)))
    assert(stack.head === Some(9))
  }

  test("tail when empty") {
    val stack = Nil
    assert(stack.tail === None)
  }

  test("tail when nonempty") {
    val stack = Cons(9, Cons(7, Cons(1, Nil)))
    assert(stack.tail === Some(Cons(7, Cons(1, Nil))))
  }

  test("cons") {
    val stack = Cons(2, Cons(20, Nil))
    assert(stack.cons(100) === Cons(100, stack))
  }

  test("append") {
    val stack1 = Cons(2, Cons(20, Nil))
    val stack2 = Cons(400, Cons(4000, Nil))
    assert(stack1.append(stack2) === Cons(2, Cons(20, Cons(400, Cons(4000, Nil)))))
  }

  test("append when first stack is empty") {
    val stack1 = Nil
    val stack2 = Cons(400, Cons(4000, Nil))
    assert(stack1.append(stack2) === stack2)
  }

  test("append when second stack is empty") {
    val stack1 = Cons(10, Cons(20, Nil))
    val stack2 = Nil
    assert(stack1.append(stack2) === stack1)
  }

  test("suffixes") {
    val stack = Cons(2, Cons(4, Cons(6, Cons(8, Nil))))
    assert(stack.suffixes === Cons(
      Cons(2, Cons(4, Cons(6, Cons(8, Nil)))), Cons(
        Cons(4, Cons(6, Cons(8, Nil))), Cons(
          Cons(6, Cons(8, Nil)), Cons(
            Cons(8, Nil), Nil)))))
  }
}
