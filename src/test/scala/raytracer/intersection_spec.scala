package raytracer

import org.scalatest.freespec.AnyFreeSpec

class intersection_spec extends AnyFreeSpec {
  "An intersection" - {
    "contains a time and an object" in {
      val s = new sphere()
      val i = new intersection(3.5, s)
      assert(utils.float_equals(i.t, 3.5))
      assert(i.`object` == s)
    }
  }
  "An intersections" - {
    "aggregates multiple intersections" in {
      val s = new sphere()
      val i1 = new intersection(1, s)
      val i2 = new intersection(2, s)
      val xs = new intersections(i1, i2)
      assert(xs.length == 2)
      assert(utils.float_equals(xs(0).t, 1))
      assert(utils.float_equals(xs(1).t, 2))
    }
    "computes hit for all intersections with positive t" in {
      val s = new sphere()
      val i1 = new intersection(1, s)
      val i2 = new intersection(2, s)
      val xs = new intersections(i2, i1)
      val i = xs.hit()
      i match {
        case Some(v) => assert(v == i1)
        case None => throw new RuntimeException("Expected a hit")
      }
    }
    "computes hit for some intersections with negative t" in {
      val s = new sphere()
      val i1 = new intersection(-1, s)
      val i2 = new intersection(1, s)
      val xs = new intersections(i2, i1)
      val i = xs.hit()
      i match {
        case Some(v) => assert(v == i2)
        case None => throw new RuntimeException("Expected a hit")
      }
    }
    "computes hit for all intersections with negative t" in {
      val s = new sphere()
      val i1 = new intersection(-2, s)
      val i2 = new intersection(-1, s)
      val xs = new intersections(i2, i1)
      val i = xs.hit()
      i match {
        case Some(v) => throw new RuntimeException("Expected no hit")
        case None => true
      }
    }
    "computes the hit as the lowest nonnegative intersection" in {
      val s = new sphere()
      val i1 = new intersection(5, s)
      val i2 = new intersection(7, s)
      val i3 = new intersection(-3, s)
      val i4 = new intersection(2, s)
      val xs = new intersections(i1, i2, i3, i4)
      val i = xs.hit()
      i match {
        case Some(v) => assert(v == i4)
        case None => throw new RuntimeException("Expected a hit")
      }
    }
  }
}
