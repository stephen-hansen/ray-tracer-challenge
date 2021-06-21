package raytracer

object utils {
  val EPSILON = 0.00001
  def float_equals(a: Double, b: Double): Boolean = {
    math.abs(a - b) < EPSILON
  }
}
