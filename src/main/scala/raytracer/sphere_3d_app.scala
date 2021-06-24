package raytracer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object sphere_3d_app extends App {
  var ray_origin = new point(0, 0, -5)
  val wall_z = 10
  val wall_size = 7.0
  val canvas_pixels = 100
  val pixel_size = wall_size / canvas_pixels
  val half = wall_size / 2
  val c = new canvas(canvas_pixels, canvas_pixels)
  val shape = new sphere()
  shape.material = new material()
  shape.material.color = new color(1, 0.2, 1)
  val light_position = new point(-10, 10, -10)
  val light_color = new color(1,1,1)
  val light = new point_light(light_position, light_color)
  for (y <- 0 until canvas_pixels) {
    val world_y = half - pixel_size * y
    for (x <- 0 until canvas_pixels) {
      val world_x = -half + pixel_size * x
      val position = new point(world_x, world_y, wall_z)
      val r = new ray(ray_origin, (position - ray_origin).normalize().to_vector())
      val xs = shape.intersect(r)
      if (xs.hit().isDefined) {
        val hit = xs.hit().get
        val point = r.position(hit.t)
        val normal = hit.`object`.normal_at(point)
        val eye = (-r.direction).to_vector()
        val color = hit.`object`.material.lighting(hit.`object`, light, point, eye, normal)
        c.write_pixel(x,y,color)
      }
    }
  }
  val ppm = c.to_ppm()
  Files.write(Paths.get("sphere_3d.ppm"), ppm.getBytes(StandardCharsets.UTF_8))
}
