package raytracer

class tuple(var x: Double, var y: Double, var z: Double, var w: Double) {
  def apply(i: Int): Double = {
    i match {
      case 0 => this.x
      case 1 => this.y
      case 2 => this.z
      case 3 => this.w
    }
  }

  def update(i: Int, item: Double) : Unit = {
    i match {
      case 0 => this.x = item
      case 1 => this.y = item
      case 2 => this.z = item
      case 3 => this.w = item
    }
  }

  def is_point() : Boolean = {
    utils.float_equals(this.w, 1.0)
  }

  def is_vector() : Boolean = {
    utils.float_equals(this.w, 0.0)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: tuple => utils.float_equals(this.x, t.x) &&
        utils.float_equals(this.y, t.y) &&
        utils.float_equals(this.z, t.z) &&
        utils.float_equals(this.w, t.w)
      case _ => false
    }
  }

  def +(that: tuple): tuple = {
    new tuple(this.x + that.x, this.y + that.y, this.z + that.z, this.w + that.w)
  }

  def -(that: tuple): tuple = {
    new tuple(this.x - that.x, this.y - that.y, this.z - that.z, this.w - that.w)
  }

  def unary_- : tuple = {
    new tuple(-this.x, -this.y, -this.z, -this.w)
  }

  def *(that: Double): tuple = {
    new tuple(this.x * that, this.y * that, this.z * that, this.w * that)
  }

  def /(that: Double): tuple = {
    new tuple(this.x / that, this.y / that, this.z / that, this.w / that)
  }

  def magnitude(): Double = {
    math.sqrt(math.pow(this.x, 2) + math.pow(this.y, 2) + math.pow(this.z, 2) + math.pow(this.w, 2))
  }

  def normalize(): tuple = {
    new tuple(this.x / this.magnitude(), this.y / this.magnitude(), this.z / this.magnitude(), this.w / this.magnitude())
  }

  def dot(that: tuple): Double = {
    this.x * that.x + this.y * that.y + this.z * that.z + this.w * that.w
  }

  def cross(that: tuple): tuple = {
    new vector(this.y * that.z - this.z * that.y, this.z * that.x - this.x * that.z, this.x * that.y - this.y * that.x)
  }

  def reflect(that: tuple): tuple = {
    this - that * 2 * (this dot that)
  }

  def view_transform(to: point, up: vector): matrix = {
    val forward = (to - this).normalize()
    val upn = up.normalize()
    val left = forward cross upn
    val true_up = left cross forward
    val orientation = new matrix(
      Array(
        Array(left.x, left.y, left.z, 0),
        Array(true_up.x, true_up.y, true_up.z, 0),
        Array(-forward.x, -forward.y, -forward.z, 0),
        Array(0,0,0,1),
      )
    )
    orientation * new translation(-this.x, -this.y, -this.z)
  }

  def to_point(): point = {
    new point(this.x, this.y, this.z)
  }

  def to_vector(): vector = {
    new vector(this.x, this.y, this.z)
  }
}

class point(x: Double, y: Double, z: Double) extends tuple(x, y, z, 1.0)

class vector(x: Double, y: Double, z: Double) extends tuple(x, y, z, 0.0)
