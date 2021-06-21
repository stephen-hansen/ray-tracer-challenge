package raytracer

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object projectile_canvas_app extends App {
  val start = new point(0, 1, 0)
  val velocity = new vector(1, 1.8, 0).normalize() * 11.25
  var p = new projectile(start, velocity.to_vector())

  val gravity = new vector(0, -0.1, 0)
  val wind = new vector(-0.01, 0, 0)
  val e = new environment(gravity, wind)

  val c = new canvas(900, 550)

  var ticks = 0
  while (p.position.y > 0) {
    println("Position: (" + p.position.x + ", " + p.position.y + ", " + p.position.z + ")")
    val canvas_x = p.position.x.round.toInt
    val canvas_y = 550 - p.position.y.round.toInt
    if (canvas_x >= 0 && canvas_x < 900 && canvas_y >= 0 && canvas_y < 550) {
      c.write_pixel(canvas_x, canvas_y, new color(1, 0, 0))
    }
    p = projectile_utils.tick(e, p)
    ticks += 1
  }

  println("Final Position: (" + p.position.x + ", " + p.position.y + ", " + p.position.z + ")")
  println("Number of Ticks: " + ticks)
  val ppm = c.to_ppm()
  Files.write(Paths.get("projectile.ppm"), ppm.getBytes(StandardCharsets.UTF_8))
}
