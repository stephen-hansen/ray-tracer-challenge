package raytracer

import org.scalatest.freespec.AnyFreeSpec

class sphere_spec extends AnyFreeSpec {
  "A sphere" - {
    "intersects a ray at two points" in {
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val s = new sphere()
      val xs = s.intersect(r)
      assert(xs.length == 2)
      assert(utils.float_equals(xs(0).t, 4.0))
      assert(utils.float_equals(xs(1).t, 6.0))
    }
    "intersects a ray at a tangent" in {
      val r = new ray(new point(0,1,-5), new vector(0,0,1))
      val s = new sphere()
      val xs = s.intersect(r)
      assert(xs.length == 2)
      assert(utils.float_equals(xs(0).t, 5.0))
      assert(utils.float_equals(xs(1).t, 5.0))
    }
    "misses a ray" in {
      val r = new ray(new point(0,2,-5), new vector(0,0,1))
      val s = new sphere()
      val xs = s.intersect(r)
      assert(xs.length == 0)
    }
    "originates with a ray" in {
      val r = new ray(new point(0,0,0), new vector(0,0,1))
      val s = new sphere()
      val xs = s.intersect(r)
      assert(xs.length == 2)
      assert(utils.float_equals(xs(0).t, -1.0))
      assert(utils.float_equals(xs(1).t, 1.0))
    }
    "is behind a ray" in {
      val r = new ray(new point(0,0,5), new vector(0,0,1))
      val s = new sphere()
      val xs = s.intersect(r)
      assert(xs.length == 2)
      assert(utils.float_equals(xs(0).t, -6.0))
      assert(utils.float_equals(xs(1).t, -4.0))
    }
    "sets intersection object" in {
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val s = new sphere()
      val xs = s.intersect(r)
      assert(xs.length == 2)
      assert(xs(0).`object` == s)
      assert(xs(1).`object` == s)
    }
    "has default transformation of identity matrix" in {
      val s = new sphere()
      assert(s.transform == matrix.identity_matrix)
    }
    "can change transformation" in {
      val s = new sphere()
      val t = new translation(2,3,4)
      s.set_transform(t)
      assert(s.transform == t)
    }
    "can be scaled and intersect a ray" in {
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val s = new sphere()
      s.set_transform(new scaling(2,2,2))
      val xs = s.intersect(r)
      assert(xs.length == 2)
      assert(utils.float_equals(xs(0).t, 3))
      assert(utils.float_equals(xs(1).t, 7))
    }
    "can be translated and intersect a ray" in {
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val s = new sphere()
      s.set_transform(new translation(5,0,0))
      val xs = s.intersect(r)
      assert(xs.length == 0)
    }
  }
}
