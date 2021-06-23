package raytracer

import org.scalatest.freespec.AnyFreeSpec

class ray_spec extends AnyFreeSpec {
  "A ray" - {
    "has origin and direction" in {
      val origin = new point(1,2,3)
      val direction = new vector(4,5,6)
      val r = new ray(origin, direction)
      assert(r.origin == origin)
      assert(r.direction == direction)
    }
    "can compute a point from a distance" in {
      val r = new ray(new point(2,3,4), new vector(1,0,0))
      assert(r.position(0) == new point(2,3,4))
      assert(r.position(1) == new point(3,3,4))
      assert(r.position(-1) == new point(1,3,4))
      assert(r.position(2.5) == new point(4.5,3,4))
    }
    "can be translated" in {
      val r = new ray(new point(1,2,3), new vector(0,1,0))
      val m = new translation(3,4,5)
      val r2 = r.transform(m)
      assert(r2.origin == new point(4,6,8))
      assert(r2.direction == new vector(0,1,0))
    }
    "can be scaled" in {
      val r = new ray(new point(1,2,3), new vector(0,1,0))
      val m = new scaling(2,3,4)
      val r2 = r.transform(m)
      assert(r2.origin == new point(2,6,12))
      assert(r2.direction == new vector(0,3,0))
    }
  }
}
