package raytracer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object sphere_app extends App {
  var ray_origin = new point(0, 0, -5)
  val wall_z = 10
  val wall_size = 7.0
  val canvas_pixels = 100
  val pixel_size = wall_size / canvas_pixels
  val half = wall_size / 2
  val c = new canvas(canvas_pixels, canvas_pixels)
  val color = new color(1,0,0)
  val shape = new sphere()
  for (y <- 0 until canvas_pixels) {
    val world_y = half - pixel_size * y
    for (x <- 0 until canvas_pixels) {
      val world_x = -half + pixel_size * x
      val position = new point(world_x, world_y, wall_z)
      val r = new ray(ray_origin, (position - ray_origin).normalize().to_vector())
      val xs = shape.intersect(r)
      if (xs.hit().isDefined) {
        c.write_pixel(x,y,color)
      }
    }
  }
  val ppm = c.to_ppm()
  Files.write(Paths.get("sphere.ppm"), ppm.getBytes(StandardCharsets.UTF_8))
}
