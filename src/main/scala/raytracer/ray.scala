package raytracer

class ray(val origin: point, val direction: vector) {
  def position(t: Double): point = {
    (this.origin + this.direction * t).to_point()
  }

  def transform(m: matrix): ray = {
    new ray((m * this.origin).to_point(), (m * this.direction).to_vector())
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case r: ray => (this.origin == r.origin) &&
        (this.direction == r.direction)
      case _ => false
    }
  }
}
