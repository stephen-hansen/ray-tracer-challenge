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
    "added to a tuple produces a tuple" in {
      val a1 = new tuple(3, -2, 5, 1)
      val a2 = new tuple(-2, 3, 1, 0)
      assert(a1 + a2 == new tuple(1, 1, 6, 1))
    }
    "negated produces a negated tuple" in {
      val a = new tuple(1, -2, 3, -4)
      assert(-a == new tuple(-1, 2, -3, 4))
    }
    "multiplied by a scalar produces a tuple" in {
      val a = new tuple(1, -2, 3, -4)
      assert(a * 3.5 == new tuple(3.5, -7, 10.5, -14))
    }
    "multiplied by a fraction produces a tuple" in {
      val a = new tuple(1, -2, 3, -4)
      assert(a * 0.5 == new tuple(0.5, -1, 1.5, -2))
    }
    "divided by a scalar produces a tuple" in {
      val a = new tuple(1, -2, 3, -4)
      assert(a / 2 == new tuple(0.5, -1, 1.5, -2))
    }
  }
  "A point" - {
    "creates tuples with w=1" in {
      val p = new point(4, -4, 3)
      assert(p == new tuple(4, -4, 3, 1))
    }
    "subtracted from a point produces a vector" in {
      val p1 = new point(3, 2, 1)
      val p2 = new point(5, 6, 7)
      assert(p1 - p2 == new vector(-2, -4, -6))
    }
  }
  "A vector" - {
    "creates tuples with w=0" in {
      val v = new vector(4, -4, 3)
      assert(v == new tuple(4, -4, 3, 0))
    }
    "subtracted from a point produces a point" in {
      val p = new point(3, 2, 1)
      val v = new vector(5, 6, 7)
      assert(p - v == new point(-2, -4, -6))
    }
    "subtracted from a vector produces a vector" in {
      val v1 = new vector(3, 2, 1)
      val v2 = new vector(5, 6, 7)
      assert(v1 - v2 == new vector(-2, -4, -6))
    }
    "subtracted from the zero vector negates it" in {
      val zero = new vector(0, 0, 0)
      val v = new vector(1, -2, 3)
      assert(zero - v == new vector(-1, 2, -3))
    }
    "has correct magnitude (1)" in {
      val v = new vector(1, 0, 0)
      assert(utils.float_equals(v.magnitude(), 1))
    }
    "has correct magnitude (2)" in {
      val v = new vector(0, 1, 0)
      assert(utils.float_equals(v.magnitude(), 1))
    }
    "has correct magnitude (3)" in {
      val v = new vector(0, 0, 1)
      assert(utils.float_equals(v.magnitude(), 1))
    }
    "has correct magnitude (4)" in {
      val v = new vector(1, 2, 3)
      assert(utils.float_equals(v.magnitude(), math.sqrt(14)))
    }
    "has correct magnitude (5)" in {
      val v = new vector(-1, -2, -3)
      assert(utils.float_equals(v.magnitude(), math.sqrt(14)))
    }
    "normalized gives (1, 0, 0)" in {
      val v = new vector(4, 0, 0)
      assert(v.normalize() == new vector(1, 0, 0))
    }
    "normalized gives approximately the correct vector" in {
      val v = new vector(1, 2, 3)
      assert(v.normalize() == new vector(0.26726, 0.53452, 0.80178))
    }
    "`dot`ted with another vector yields a scalar" in {
      val a = new vector(1, 2, 3)
      val b = new vector(2, 3, 4)
      assert(utils.float_equals(a dot b, 20))
    }
    "`cross`ed with another vector yields a vector" in {
      val a = new vector(1, 2, 3)
      val b = new vector(2, 3, 4)
      assert((a cross b) == new vector(-1, 2, -1))
      assert((b cross a) == new vector(1, -2, 1))
    }
  }
}
