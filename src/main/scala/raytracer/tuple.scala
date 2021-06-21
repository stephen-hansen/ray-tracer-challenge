package raytracer

class tuple(val x: Double, val y: Double, val z: Double, val w: Double) {
  def is_point() : Boolean = {
    utils.float_equals(w, 1.0)
  }

  def is_vector() : Boolean = {
    utils.float_equals(w, 0.0)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: tuple => utils.float_equals(this.x, t.x) &&
        utils.float_equals(this.y, t.y) &&
        utils.float_equals(this.z, t.z) &&
        utils.float_equals(this.w, t.w)
    }
  }
}

class point(x: Double, y: Double, z: Double) extends tuple(x, y, z, 1.0)

class vector(x: Double, y: Double, z: Double) extends tuple(x, y, z, 0.0)
