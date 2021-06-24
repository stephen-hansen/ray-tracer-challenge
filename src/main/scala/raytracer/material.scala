package raytracer

class material {
  var color: color = new color(1,1,1)
  var ambient: Double = 0.1
  var diffuse: Double = 0.9
  var specular: Double = 0.9
  var shininess: Double = 200.0
  var pattern: Option[pattern] = None

  override def equals(obj: Any): Boolean = {
    obj match {
      case m: material => (this.color == m.color) &&
        utils.float_equals(this.ambient, m.ambient) &&
        utils.float_equals(this.diffuse, m.diffuse) &&
        utils.float_equals(this.specular, m.specular) &&
        utils.float_equals(this.shininess, m.shininess) &&
        (this.pattern == m.pattern)
      case _ => false
    }
  }

  def lighting(`object`: shape, light: point_light, point: point, eyev: vector, normalv: vector, in_shadow: Boolean = false) : color = {
    var color = new color(0,0,0)
    if (this.pattern.isDefined) {
      color = this.pattern.get.pattern_at_shape(`object`, point)
    } else {
      color = this.color
    }
    val effective_color = color * light.intensity
    val lightv = (light.position - point).normalize()
    val ambient = effective_color * this.ambient
    if (in_shadow) {
      return ambient
    }
    val light_dot_normal = lightv dot normalv
    var diffuse = new color(0,0,0)
    var specular = new color(0,0,0)
    if (light_dot_normal < 0) {
      diffuse = new color(0,0,0)
      specular = new color(0,0,0)
    } else {
      diffuse = effective_color * this.diffuse * light_dot_normal
      val reflectv = -lightv.reflect(normalv)
      val reflect_dot_eye = reflectv dot eyev
      if (reflect_dot_eye <= 0) {
        specular = new color(0,0,0)
      } else {
        val factor = math.pow(reflect_dot_eye, this.shininess)
        specular = light.intensity * this.specular * factor
      }
    }
    ambient + diffuse + specular
  }
}
