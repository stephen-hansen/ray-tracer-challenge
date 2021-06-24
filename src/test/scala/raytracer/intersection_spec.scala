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
    "precomputes the state of an intersection" in {
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val shape = new sphere()
      val i = new intersection(4, shape)
      val comps = i.prepare_computations(r)
      assert(comps.t.isDefined)
      assert(comps.`object`.isDefined)
      assert(comps.point.isDefined)
      assert(comps.eyev.isDefined)
      assert(comps.normalv.isDefined)
      assert(utils.float_equals(comps.t.get, i.t))
      assert(comps.`object`.get == i.`object`)
      assert(comps.point.get == new point(0,0,-1))
      assert(comps.eyev.get == new vector(0,0,-1))
      assert(comps.normalv.get == new vector(0,0,-1))
    }
    "precomputes an intersection on the outside" in {
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val shape = new sphere()
      val i = new intersection(4, shape)
      val comps = i.prepare_computations(r)
      assert(comps.inside.isDefined)
      assert(!comps.inside.get)
    }
    "precomputes an intersection on the inside" in {
      val r = new ray(new point(0,0,0), new vector(0,0,1))
      val shape = new sphere()
      val i = new intersection(1, shape)
      val comps = i.prepare_computations(r)
      assert(comps.point.isDefined)
      assert(comps.eyev.isDefined)
      assert(comps.inside.isDefined)
      assert(comps.normalv.isDefined)
      assert(comps.point.get == new point(0,0,1))
      assert(comps.eyev.get == new vector(0,0,-1))
      assert(comps.inside.get)
      assert(comps.normalv.get == new vector(0,0,-1))
    }
    "precomputes an offset point" in {
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val shape = new sphere()
      shape.set_transform(new translation(0,0,1))
      val i = new intersection(5,shape)
      val comps = i.prepare_computations(r)
      assert(comps.over_point.isDefined)
      assert(comps.over_point.get.z < -utils.EPSILON/2)
      assert(comps.point.isDefined)
      assert(comps.point.get.z > comps.over_point.get.z)
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
        case Some(_) => throw new RuntimeException("Expected no hit")
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
