package raytracer

import org.scalatest.freespec.AnyFreeSpec

class color_spec extends AnyFreeSpec {
  "A color" - {
    "is a (red, green, blue) tuple" in {
      val c = new color(-0.5, 0.4, 1.7)
      assert(utils.float_equals(c.red, -0.5))
      assert(utils.float_equals(c.green, 0.4))
      assert(utils.float_equals(c.blue, 1.7))
    }
    "added to a color is a color" in {
      val c1 = new color(0.9, 0.6, 0.75)
      val c2 = new color(0.7, 0.1, 0.25)
      assert(c1 + c2 == new color(1.6, 0.7, 1.0))
    }
    "subtracted from a color is a color" in {
      val c1 = new color(0.9, 0.6, 0.75)
      val c2 = new color(0.7, 0.1, 0.25)
      assert(c1 - c2 == new color(0.2, 0.5, 0.5))
    }
    "multiplied by a scalar is a color" in {
      val c = new color(0.2, 0.3, 0.4)
      assert(c * 2 == new color(0.4, 0.6, 0.8))
    }
    "multiplied by a color is a color" in {
      val c1 = new color(1, 0.2, 0.4)
      val c2 = new color(0.9, 1, 0.1)
      assert(c1 * c2 == new color(0.9, 0.2, 0.04))
    }
  }
}
