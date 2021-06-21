package raytracer

import org.scalatest.freespec.AnyFreeSpec

class canvas_spec extends AnyFreeSpec {
  "A canvas" - {
    "has width, height, and is initialized to black" in {
      val c = new canvas(10, 20)
      assert(c.width == 10)
      assert(c.height == 20)
      // Every pixel is black
      for (y <- 0 until 20) {
        for (x <- 0 until 10) {
          assert(c.pixel_at(x, y) == new color(0, 0, 0))
        }
      }
    }
    "can write a color at a given coordinate" in {
      val c = new canvas(10, 20)
      val red = new color(1, 0, 0)
      c.write_pixel(2, 3, red)
      assert(c.pixel_at(2, 3) == red)
    }
    "can construct PPM header" in {
      val c = new canvas(5, 3)
      val ppm = c.to_ppm()
      val lines = ppm.split("\n")
      assert(lines(0) == "P3")
      assert(lines(1) == "5 3")
      assert(lines(2) == "255")
    }
    "can construct PPM pixel data" in {
      val c = new canvas(5, 3)
      val c1 = new color(1.5, 0, 0)
      val c2 = new color(0, 0.5, 0)
      val c3 = new color(-0.5, 0, 1)
      c.write_pixel(0, 0, c1)
      c.write_pixel(2, 1, c2)
      c.write_pixel(4, 2, c3)
      val ppm = c.to_ppm()
      val lines = ppm.split("\n")
      assert(lines(3) == "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0")
      assert(lines(4) == "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0")
      assert(lines(5) == "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255")
    }
    "can properly split long lines in PPM files" in {
      val c = new canvas(10, 2)
      for (y <- 0 until 2) {
        for (x <- 0 until 10) {
          c.write_pixel(x, y, new color(1, 0.8, 0.6))
        }
      }
      val ppm = c.to_ppm()
      val lines = ppm.split("\n")
      assert(lines(3) == "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204")
      assert(lines(4) == "153 255 204 153 255 204 153 255 204 153 255 204 153")
      assert(lines(5) == "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204")
      assert(lines(6) == "153 255 204 153 255 204 153 255 204 153 255 204 153")
    }
    "terminates PPM file with newline" in {
      val c = new canvas(5, 3)
      val ppm = c.to_ppm()
      assert(ppm.endsWith("\n"))
    }
  }
}
