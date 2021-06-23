package raytracer

abstract class shape {
  val id: String = java.util.UUID.randomUUID().toString // See page 59-60, needs to be unique
  var material: material = new material()
  var transform: matrix = matrix.identity_matrix
  def local_normal_at(p: point): vector
  def normal_at(p: point): vector = {
    val local_point = this.transform.inverse() * p
    val local_normal = this.local_normal_at(local_point.to_point())
    val world_normal = this.transform.inverse().transpose() * local_normal
    world_normal.w = 0
    world_normal.normalize().to_vector()
  }
  def local_intersect(r: ray): intersections
  def intersect(r: ray): intersections = {
    val local_ray = r.transform(this.transform.inverse())
    this.local_intersect(local_ray)
  }

  def set_transform(t: matrix): Unit = {
    this.transform = t
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case s: shape => (this.material == s.material) &&
        (this.transform == s.transform)
      case _ => false
    }
  }
}

class test_shape extends shape {
  var saved_ray: Option[ray] = None
  override def local_intersect(r: ray): intersections = {
    this.saved_ray = Some(r)
    new intersections()
  }

  override def local_normal_at(p: point): vector = {
    new vector(p.x, p.y, p.z)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case _: test_shape => super.equals(obj)
      case _ => false
    }
  }
}

class sphere extends shape {
  override def local_intersect(r: ray): intersections = {
    val sphere_to_ray = r.origin - new point(0,0,0)
    val a = r.direction dot r.direction
    val b = 2 * (r.direction dot sphere_to_ray)
    val c = (sphere_to_ray dot sphere_to_ray) - 1
    val discriminant = math.pow(b,2) - 4*a*c
    if (discriminant < 0) {
      return new intersections()
    }
    val t1 = (-b - math.sqrt(discriminant)) / (2*a)
    val t2 = (-b + math.sqrt(discriminant)) / (2*a)
    new intersections(new intersection(t1,this), new intersection(t2,this))
  }

  override def local_normal_at(p: point): vector = {
    (p - new point(0,0,0)).to_vector()
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case _: sphere => super.equals(obj)
      case _ => false
    }
  }
}

class plane extends shape {
  override def local_intersect(r: ray): intersections = {
    if (math.abs(r.direction.y) < utils.EPSILON) {
      return new intersections()
    }
    val t = -r.origin.y / r.direction.y
    new intersections(new intersection(t, this))
  }

  override def local_normal_at(p: point): vector = {
    new vector(0,1,0)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case _: plane => super.equals(obj)
      case _ => false
    }
  }
}
