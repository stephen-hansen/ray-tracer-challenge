package raytracer

import org.scalatest.freespec.AnyFreeSpec

class tuple_spec extends AnyFreeSpec {
  "A tuple" - {
    "with w=1.0 is a point" in {
      val a = new tuple(4.3, -4.2, 3.1, 1.0)
      assert(utils.float_equals(a.x, 4.3))
      assert(utils.float_equals(a.y, -4.2))
      assert(utils.float_equals(a.z, 3.1))
      assert(utils.float_equals(a.w, 1.0))
      assert(a.is_point())
      assert(!a.is_vector())
    }
    "with w=0 is a vector" in {
      val a = new tuple(4.3, -4.2, 3.1, 0.0)
      assert(utils.float_equals(a.x, 4.3))
      assert(utils.float_equals(a.y, -4.2))
      assert(utils.float_equals(a.z, 3.1))
      assert(utils.float_equals(a.w, 0.0))
      assert(!a.is_point())
      assert(a.is_vector())
    }
  }
  "A point" - {
    "creates tuples with w=1" in {
      val p = new point(4, -4, 3)
      assert(p == new tuple(4, -4, 3, 1))
    }
  }
  "A vector" - {
    "creates tuples with w=0" in {
      val v = new vector(4, -4, 3)
      assert(v == new tuple(4, -4, 3, 0))
    }
  }
}
