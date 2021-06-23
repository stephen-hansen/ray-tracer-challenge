package raytracer

import org.scalatest.freespec.AnyFreeSpec

class material_spec extends AnyFreeSpec {
  "A material" - {
    "has default parameters" in {
      val m = new material()
      assert(m.color == new color(1,1,1))
      assert(utils.float_equals(m.ambient, 0.1))
      assert(utils.float_equals(m.diffuse, 0.9))
      assert(utils.float_equals(m.specular, 0.9))
      assert(utils.float_equals(m.shininess, 200.0))
    }
    val m = new material()
    val position = new point(0,0,0)
    "has lighting between the light and the surface" in {
      val eyev = new vector(0,0,-1)
      val normalv = new vector(0,0,-1)
      val light = new point_light(new point(0,0,-10), new color(1,1,1))
      val result = m.lighting(light, position, eyev, normalv)
      assert(result == new color(1.9,1.9,1.9))
    }
    "has lighting between the light and the surface, eye offset 45 deg" in {
      val eyev = new vector(0,math.sqrt(2)/2,-math.sqrt(2)/2)
      val normalv = new vector(0,0,-1)
      val light = new point_light(new point(0,0,-10), new color(1,1,1))
      val result = m.lighting(light, position, eyev, normalv)
      assert(result == new color(1.0,1.0,1.0))
    }
    "has lighting with eye opposite surface, light offset 45 deg" in {
      val eyev = new vector(0,0,-1)
      val normalv = new vector(0,0,-1)
      val light = new point_light(new point(0,10,-10), new color(1,1,1))
      val result = m.lighting(light, position, eyev, normalv)
      assert(result == new color(0.7364,0.7364,0.7364))
    }
    "has lighting with eye in path of reflection vector" in {
      val eyev = new vector(0,-math.sqrt(2)/2,-math.sqrt(2)/2)
      val normalv = new vector(0,0,-1)
      val light = new point_light(new point(0,10,-10), new color(1,1,1))
      val result = m.lighting(light, position, eyev, normalv)
      assert(result == new color(1.6364,1.6364,1.6364))
    }
    "has lighting with light behind surface" in {
      val eyev = new vector(0,0,-1)
      val normalv = new vector(0,0,-1)
      val light = new point_light(new point(0,0,10), new color(1,1,1))
      val result = m.lighting(light, position, eyev, normalv)
      assert(result == new color(0.1,0.1,0.1))
    }
  }
}