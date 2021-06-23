package raytracer

class point_light(val position: point, val intensity: color) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case p: point_light => (this.position == p.position) &&
        (this.intensity == p.intensity)
      case _ => false
    }
  }
}
