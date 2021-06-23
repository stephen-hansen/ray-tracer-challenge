package raytracer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object world_app extends App {
  val floor = new sphere()
  floor.transform = new scaling(10,0.01,10)
  floor.material = new material()
  floor.material.color = new color(1, 0.9, 0.9)
  floor.material.specular = 0

  val left_wall = new sphere()
  left_wall.transform = new translation(0,0,5) * new rotation_y(-math.Pi/4) * new rotation_x(math.Pi/2) * new scaling(10,0.01,10)
  left_wall.material = floor.material

  val right_wall = new sphere()
  right_wall.transform = new translation(0,0,5) * new rotation_y(math.Pi/4) * new rotation_x(math.Pi/2) * new scaling(10,0.01,10)
  right_wall.material = floor.material

  val middle = new sphere()
  middle.transform = new translation(-0.5,1,0.5)
  middle.material = new material()
  middle.material.color = new color(0.1,1,0.5)
  middle.material.diffuse = 0.7
  middle.material.specular = 0.3

  val right = new sphere()
  right.transform = new translation(1.5,0.5,-0.5) * new scaling(0.5,0.5,0.5)
  right.material = new material()
  right.material.color = new color(0.5,1,0.1)
  right.material.diffuse = 0.7
  right.material.specular = 0.3

  val left = new sphere()
  left.transform = new translation(-1.5,0.33,-0.75) * new scaling(0.33,0.33,0.33)
  left.material = new material()
  left.material.color = new color(1,0.8,0.1)
  left.material.diffuse = 0.7
  left.material.specular = 0.3

  val world = new world()
  world.light = Some(new point_light(new point(-10,10,-10), new color(1,1,1)))
  world.add_object(floor)
  world.add_object(left_wall)
  world.add_object(right_wall)
  world.add_object(middle)
  world.add_object(right)
  world.add_object(left)

  val camera = new camera(1000, 500, math.Pi/3)
  camera.transform = new point(0,1.5,-5).view_transform(new point(0,1,0), new vector(0,1,0))

  val canvas = camera.render(world)
  val ppm = canvas.to_ppm()
  Files.write(Paths.get("world.ppm"), ppm.getBytes(StandardCharsets.UTF_8))
}
