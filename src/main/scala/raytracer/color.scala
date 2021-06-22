package raytracer

class color(val red: Double, val green: Double, val blue: Double) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case c: color => utils.float_equals(this.red, c.red) &&
        utils.float_equals(this.green, c.green) &&
        utils.float_equals(this.blue, c.blue)
      case _ => false
    }
  }

  def +(that: color): color = {
    new color(this.red + that.red, this.green + that.green, this.blue + that.blue)
  }

  def -(that: color): color = {
    new color(this.red - that.red, this.green - that.green, this.blue - that.blue)
  }

  def *(that: Double): color = {
    new color(this.red * that, this.green * that, this.blue * that)
  }

  def *(that: color): color = {
    new color(this.red * that.red, this.green * that.green, this.blue * that.blue)
  }
}
