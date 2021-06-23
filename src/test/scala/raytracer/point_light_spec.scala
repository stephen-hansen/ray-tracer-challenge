package raytracer

import org.scalatest.freespec.AnyFreeSpec

class point_light_spec extends AnyFreeSpec {
  "A point light" - {
    "has intensity and position" in {
      val intensity = new color(1,1,1)
      val position = new point(0,0,0)
      val light = new point_light(position, intensity)
      assert(light.position == position)
      assert(light.intensity == intensity)
    }
  }
}
