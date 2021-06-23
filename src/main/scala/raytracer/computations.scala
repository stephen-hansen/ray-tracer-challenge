package raytracer

class computations() {
  var t: Option[Double] = None
  var `object`: Option[shape] = None
  var point: Option[point] = None
  var eyev: Option[vector] = None
  var normalv: Option[vector] = None
  var inside: Option[Boolean] = None
  var over_point: Option[point] = None
  override def equals(obj: Any): Boolean = {
    obj match {
      case c: computations => ((this.t.isEmpty && c.t.isEmpty) || utils.float_equals(this.t.get, c.t.get)) &&
        (this.`object` == c.`object`) &&
        (this.point == c.point) &&
        (this.eyev == c.eyev) &&
        (this.normalv == c.normalv) &&
        (this.inside == c.inside) &&
        (this.over_point == c.over_point)
      case _ => false
    }
  }
}
