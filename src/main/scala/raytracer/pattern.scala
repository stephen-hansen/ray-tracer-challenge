package raytracer

abstract class pattern {
  var transform: matrix = matrix.identity_matrix
  def pattern_at(p: point): color

  def pattern_at_shape(`object`: shape, world_point: point): color = {
    val object_point = `object`.transform.inverse() * world_point
    val pattern_point = this.transform.inverse() * object_point
    this.pattern_at(pattern_point.to_point())
  }

  def set_pattern_transform(m: matrix): Unit = {
    this.transform = m
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: pattern => this.transform == p.transform
      case _ => false
    }
  }
}

class test_pattern extends pattern {
  override def pattern_at(p: point): color = {
    new color(p.x, p.y, p.z)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case _: test_pattern => super.equals(obj)
      case _ => false
    }
  }
}

class stripe_pattern(val a: color, val b: color) extends pattern {
  override def pattern_at(p: point): color = {
    if (math.floor(p.x) % 2 == 0) this.a else this.b
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: stripe_pattern => (this.a == p.a) &&
        (this.b == p.b) &&
        super.equals(obj)
      case _ => false
    }
  }
}

class gradient_pattern(val a: color, val b: color) extends pattern {
  override def pattern_at(p: point): color = {
    val distance = this.b - this.a
    val fraction = p.x - math.floor(p.x)
    this.a + (distance * fraction)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: gradient_pattern => (this.a == p.a) &&
        (this.b == p.b) &&
        super.equals(obj)
      case _ => false
    }
  }
}

class ring_pattern(val a: color, val b: color) extends pattern {
  override def pattern_at(p: point): color = {
    if (math.floor(math.sqrt(math.pow(p.x, 2) + math.pow(p.z, 2))) % 2 == 0) this.a else this.b
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: ring_pattern => (this.a == p.a) &&
        (this.b == p.b) &&
        super.equals(obj)
      case _ => false
    }
  }
}

class checkers_pattern(val a: color, val b: color) extends pattern {
  override def pattern_at(p: point): color = {
    if ((math.floor(p.x) + math.floor(p.y) + math.floor(p.z)) % 2 == 0) this.a else this.b
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: checkers_pattern => (this.a == p.a) &&
        (this.b == p.b) &&
        super.equals(obj)
      case _ => false
    }
  }
}

class radial_gradient_pattern(val a: color, val b: color) extends pattern {
  override def pattern_at(p: point): color = {
    val distance = this.b - this.a
    val fraction = math.sqrt(math.pow(p.x, 2) + math.pow(p.z, 2)) - math.floor(math.sqrt(math.pow(p.x, 2) + math.pow(p.z, 2)))
    this.a + (distance * fraction)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: radial_gradient_pattern => (this.a == p.a) &&
        (this.b == p.b) &&
        super.equals(obj)
      case _ => false
    }
  }
}

class blended_pattern(val a: pattern, val b: pattern) extends pattern {
  override def pattern_at(p: point): color = {
    (this.a.pattern_at((this.a.transform.inverse() * p).to_point()) + this.b.pattern_at((this.b.transform.inverse() * p).to_point())) * 0.5
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: blended_pattern => (this.a == p.a) &&
        (this.b == p.b) &&
        super.equals(obj)
      case _ => false
    }
  }
}
