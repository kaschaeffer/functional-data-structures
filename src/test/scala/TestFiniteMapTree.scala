import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Created by schaeffer on 12/29/14.
 */

@RunWith(classOf[JUnitRunner])
class TestFiniteMapTree extends FunSuite {
  test("empty when map is empty") {
    assert(MapEmpty.empty === true)
  }

  test("empty when map is not empty") {
    assert(MapTree(1, 2, MapEmpty, MapEmpty).empty === false)
  }

  test("insert when map is empty") {
    assert(MapEmpty.insert("a", 1) === MapTree("a", 1, MapEmpty, MapEmpty))
  }

  test("insert when map is not empty") {
    val map = MapTree("k", 1,
      MapTree("b", 100, MapEmpty, MapEmpty),
      MapTree("y", 1000, MapEmpty, MapEmpty))
    val expectedMap = MapTree("k", 1,
      MapTree("b", 100, MapEmpty, MapTree("c", 9000, MapEmpty, MapEmpty)),
      MapTree("y", 1000, MapEmpty, MapEmpty))
    assert(map.insert("c", 9000) === expectedMap)
  }

  test("insert over existing key") {
    val map = MapTree("k", 1,
      MapTree("b", 100, MapEmpty, MapEmpty),
      MapTree("y", 1000, MapEmpty, MapEmpty))
    val expectedMap = MapTree("k", 1,
      MapTree("b", 100, MapEmpty, MapEmpty),
      MapTree("y", -10, MapEmpty, MapEmpty))
    assert(map.insert("y", -10) === expectedMap)
  }

  test("lookup for missing key") {
    val map = MapTree("k", 1,
      MapTree("b", 100, MapEmpty, MapEmpty),
      MapTree("y", 1000, MapEmpty, MapEmpty))
    assert(map.lookup("z") === None)
  }

  test("lookup for present key") {
    val map = MapTree("k", 1,
      MapTree("b", 100, MapEmpty, MapEmpty),
      MapTree("y", 1000, MapEmpty, MapEmpty))
    assert(map.lookup("b") === Some(100))
  }
}
