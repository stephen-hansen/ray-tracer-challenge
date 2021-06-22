package raytracer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object clock_app extends App {
  val c = new canvas(100, 100)
  var p = new point(0, 0, 1)
  val r = new rotation_y(math.Pi/6)
  val center = new point(50,0,50)
  for (_ <- 0 until 12) {
    val write_coord = (p * ((3.0/8.0) * 100.0)) + center
    println(write_coord.x)
    println(write_coord.z)
    c.write_pixel(write_coord.x.round.toInt, write_coord.z.round.toInt, new color(1,1,1))
    p = (r * p).to_point()
  }
  val ppm = c.to_ppm()
  Files.write(Paths.get("clock.ppm"), ppm.getBytes(StandardCharsets.UTF_8))
}
