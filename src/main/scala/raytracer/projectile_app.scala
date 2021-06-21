package raytracer

class projectile(val position: point, val velocity: vector)

class environment(val gravity: vector, val wind: vector)

object projectile_utils {
  def tick(env: environment, proj: projectile): projectile = {
    val position = proj.position + proj.velocity
    val velocity = proj.velocity + env.gravity + env.wind
    new projectile(position.to_point(), velocity.to_vector())
  }
}

object projectile_app extends App {
  var p = new projectile(new point(0, 1, 0), new vector(1, 1, 0).normalize().to_vector())
  val e = new environment(new vector(0, -0.1, 0), new vector(-0.01, 0, 0))
  var ticks = 0
  while (p.position.y > 0) {
    println("Position: (" + p.position.x + ", " + p.position.y + ", " + p.position.z + ")")
    p = projectile_utils.tick(e, p)
    ticks += 1
  }
  println("Final Position: (" + p.position.x + ", " + p.position.y + ", " + p.position.z + ")")
  println("Number of Ticks: " + ticks)
}
