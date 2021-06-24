package raytracer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object world_app extends App {
  val floor = new plane()
  //floor.transform = new scaling(10,0.01,10)
  floor.material = new material()
  floor.material.color = new color(1, 0.9, 0.9)
  floor.material.pattern = Some(new blended_pattern(new checkers_pattern(color.white, color.black), new ring_pattern(color.white, color.black)))
  floor.material.specular = 0

  val middle = new sphere()
  middle.transform = new translation(-0.5,1,0.5) * new rotation_x(math.Pi/2)
  middle.material = new material()
  middle.material.color = new color(0.1,1,0.5)
  middle.material.pattern = Some(new radial_gradient_pattern(new color(1,0,0), new color(0,0,1)))
  middle.material.pattern.get.set_pattern_transform(new scaling(0.1,0.1,0.1))
  middle.material.diffuse = 0.7
  middle.material.specular = 0.3

  val right = new sphere()
  right.transform = new translation(1.5,0.5,-0.5) * new scaling(0.5,0.5,0.5)
  right.material = new material()
  right.material.color = new color(0.5,1,0.1)
  right.material.pattern = Some(new gradient_pattern(new color(1,0,0), new color(0,0,1)))
  right.material.pattern.get.set_pattern_transform(new translation(0.75,0,0) * new scaling(3,3,3))
  right.material.diffuse = 0.7
  right.material.specular = 0.3

  val left = new sphere()
  left.transform = new translation(-1.5,0.33,-0.75) * new scaling(0.33,0.33,0.33)
  left.material = new material()
  left.material.color = new color(1,0.8,0.1)
  left.material.pattern = Some(new stripe_pattern(new color(1,0,0), new color(0,0,1)))
  left.material.diffuse = 0.7
  left.material.specular = 0.3

  val world = new world()
  world.light = Some(new point_light(new point(-10,10,-10), new color(1,1,1)))
  world.add_object(floor)
  world.add_object(middle)
  world.add_object(right)
  world.add_object(left)

  val camera = new camera(1000, 500, math.Pi/3)
  camera.transform = new point(0,1.5,-5).view_transform(new point(0,1,0), new vector(0,1,0))

  val canvas = camera.render(world)
  val ppm = canvas.to_ppm()
  Files.write(Paths.get("world.ppm"), ppm.getBytes(StandardCharsets.UTF_8))
}
