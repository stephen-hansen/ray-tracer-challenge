package raytracer

import org.scalatest.freespec.AnyFreeSpec

class camera_spec extends AnyFreeSpec {
  "A camera" - {
    "has hsize, vsize, field_of_view, and tranform" in {
      val hsize = 160
      val vsize = 120
      val field_of_view = math.Pi/2
      val c = new camera(hsize, vsize, field_of_view)
      assert(c.hsize == 160)
      assert(c.vsize == 120)
      assert(utils.float_equals(c.field_of_view, math.Pi/2))
      assert(c.transform == matrix.identity_matrix)
    }
    "has pixel size for a horizontal canvas" in {
      val c = new camera(200, 125, math.Pi/2)
      assert(utils.float_equals(c.pixel_size, 0.01))
    }
    "has pixel size for a vertical canvas" in {
      val c = new camera(125, 200, math.Pi/2)
      assert(utils.float_equals(c.pixel_size, 0.01))
    }
    "can construct a ray through the center of the canvas" in {
      val c = new camera(201, 101, math.Pi/2)
      val r = c.ray_for_pixel(100, 50)
      assert(r.origin == new point(0,0,0))
      assert(r.direction == new vector(0,0,-1))
    }
    "can construct a ray through the corner of the canvas" in {
      val c = new camera(201, 101, math.Pi/2)
      val r = c.ray_for_pixel(0, 0)
      assert(r.origin == new point(0,0,0))
      assert(r.direction == new vector(0.66519, 0.33259, -0.66851))
    }
    "can construct a ray when the camera is transformed" in {
      val c = new camera(201, 101, math.Pi/2)
      c.transform = new rotation_y(math.Pi/4) * new translation(0, -2, 5)
      val r = c.ray_for_pixel(100, 50)
      assert(r.origin == new point(0,2,-5))
      assert(r.direction == new vector(math.sqrt(2)/2,0,-math.sqrt(2)/2))
    }
    "can render a world" in {
      val w = world.default_world()
      val c = new camera(11,11,math.Pi/2)
      val from = new point(0,0,-5)
      val to = new point(0,0,0)
      val up = new vector(0,1,0)
      c.transform = from.view_transform(to,up)
      val image = c.render(w)
      assert(image.pixel_at(5,5) == new color(0.38066,0.47583,0.2855))
    }
  }
}
