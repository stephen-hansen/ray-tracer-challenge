package raytracer

class camera(val hsize: Int, val vsize: Int, val field_of_view: Double) {
  var transform: matrix = matrix.identity_matrix
  var half_width: Double = 0
  var half_height: Double = 0
  var pixel_size: Double = {
    val half_view = math.tan(this.field_of_view / 2)
    val aspect = this.hsize.toDouble / this.vsize.toDouble
    if (aspect >= 1) {
      this.half_width = half_view
      this.half_height = half_view / aspect
    } else {
      this.half_width = half_view * aspect
      this.half_height = half_view
    }
    (this.half_width * 2) / this.hsize
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case c: camera => (this.hsize == c.hsize) &&
        (this.vsize == c.vsize) &&
        utils.float_equals(this.field_of_view, c.field_of_view) &&
        (this.transform == c.transform)
      case _ => false
    }
  }

  def ray_for_pixel(px: Int, py: Int): ray = {
    val xoffset = (px + 0.5) * this.pixel_size
    val yoffset = (py + 0.5) * this.pixel_size
    val world_x = this.half_width - xoffset
    val world_y = this.half_height - yoffset
    val pixel = this.transform.inverse() * new point(world_x, world_y, -1)
    val origin = this.transform.inverse() * new point(0, 0, 0)
    val direction = (pixel - origin).normalize()
    new ray(origin.to_point(), direction.to_vector())
  }

  def render(world: world): canvas = {
    val image = new canvas(this.hsize, this.vsize)
    for (y <- 0 until this.vsize) {
      for (x <- 0 until this.hsize) {
        val ray = this.ray_for_pixel(x,y)
        val color = world.color_at(ray)
        image.write_pixel(x,y,color)
      }
    }
    image
  }
}
