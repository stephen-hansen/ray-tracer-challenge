package raytracer

import org.scalatest.freespec.AnyFreeSpec

class pattern_spec extends AnyFreeSpec {
  "A test pattern" - {
    "has default transformation of identity matrix" in {
      val pattern = new test_pattern()
      assert(pattern.transform == matrix.identity_matrix)
    }
    "can be assigned a transformation" in {
      val pattern = new test_pattern()
      pattern.set_pattern_transform(new translation(1,2,3))
      assert(pattern.transform == new translation(1,2,3))
    }
    "correctly computes pattern with object transformation" in {
      val shape = new sphere()
      shape.set_transform(new scaling(2,2,2))
      val pattern = new test_pattern()
      val c = pattern.pattern_at_shape(shape, new point(2,3,4))
      assert(c == new color(1,1.5,2))
    }
    "correctly computes pattern with pattern transformation" in {
      val shape = new sphere()
      val pattern = new test_pattern()
      pattern.set_pattern_transform(new scaling(2,2,2))
      val c = pattern.pattern_at_shape(shape, new point(2,3,4))
      assert(c == new color(1,1.5,2))
    }
    "correctly computes pattern with object and pattern transformation" in {
      val shape = new sphere()
      shape.set_transform(new scaling(2,2,2))
      val pattern = new test_pattern()
      pattern.set_pattern_transform(new translation(0.5,1,1.5))
      val c = pattern.pattern_at_shape(shape, new point(2.5,3,3.5))
      assert(c == new color(0.75,0.5,0.25))
    }
  }
  "A stripe pattern" - {
    "has two colors" in {
      val pattern = new stripe_pattern(color.white, color.black)
      assert(pattern.a == color.white)
      assert(pattern.b == color.black)
    }
    "is constant in y" in {
      val pattern = new stripe_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(0,1,0)) == color.white)
      assert(pattern.pattern_at(new point(0,2,0)) == color.white)
    }
    "is constant in z" in {
      val pattern = new stripe_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(0,0,1)) == color.white)
      assert(pattern.pattern_at(new point(0,0,2)) == color.white)
    }
    "alternates in x" in {
      val pattern = new stripe_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(0.9,0,0)) == color.white)
      assert(pattern.pattern_at(new point(1,0,0)) == color.black)
      assert(pattern.pattern_at(new point(-0.1,0,0)) == color.black)
      assert(pattern.pattern_at(new point(-1,0,0)) == color.black)
      assert(pattern.pattern_at(new point(-1.1,0,0)) == color.white)
    }
    "has stripes with object transformation" in {
      val `object` = new sphere()
      `object`.set_transform(new scaling(2,2,2))
      val pattern = new stripe_pattern(color.white, color.black)
      val c = pattern.pattern_at_shape(`object`, new point(1.5,0,0))
      assert(c == color.white)
    }
    "has stripes with pattern transformation" in {
      val `object` = new sphere()
      val pattern = new stripe_pattern(color.white, color.black)
      pattern.set_pattern_transform(new scaling(2,2,2))
      val c = pattern.pattern_at_shape(`object`, new point(1.5,0,0))
      assert(c == color.white)
    }
    "has stripes with object and pattern transformation" in {
      val `object` = new sphere()
      `object`.set_transform(new scaling(2,2,2))
      val pattern = new stripe_pattern(color.white, color.black)
      pattern.set_pattern_transform(new translation(0.5,0,0))
      val c = pattern.pattern_at_shape(`object`, new point(2.5,0,0))
      assert(c == color.white)
    }
  }
  "A gradient pattern" - {
    "linearly interpolates between colors" in {
      val pattern = new gradient_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(0.25,0,0)) == new color(0.75,0.75,0.75))
      assert(pattern.pattern_at(new point(0.5,0,0)) == new color(0.5,0.5,0.5))
      assert(pattern.pattern_at(new point(0.75,0,0)) == new color(0.25,0.25,0.25))
    }
  }
  "A ring pattern" - {
    "should extend in both x and z" in {
      val pattern = new ring_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(1,0,0)) == color.black)
      assert(pattern.pattern_at(new point(0,0,1)) == color.black)
      assert(pattern.pattern_at(new point(0.708,0,0.708)) == color.black)
    }
  }
  "A checkers pattern" - {
    "should repeat in x" in {
      val pattern = new checkers_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(0.99,0,0)) == color.white)
      assert(pattern.pattern_at(new point(1.01,0,0)) == color.black)
    }
    "should repeat in y" in {
      val pattern = new checkers_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(0,0.99,0)) == color.white)
      assert(pattern.pattern_at(new point(0,1.01,0)) == color.black)
    }
    "should repeat in z" in {
      val pattern = new checkers_pattern(color.white, color.black)
      assert(pattern.pattern_at(new point(0,0,0)) == color.white)
      assert(pattern.pattern_at(new point(0,0,0.99)) == color.white)
      assert(pattern.pattern_at(new point(0,0,1.01)) == color.black)
    }
  }
}
