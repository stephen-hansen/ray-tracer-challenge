package raytracer

abstract class scene_object {
  var material: material = new material()
  def normal_at(p: point): vector
}

class sphere extends scene_object {
  val id: String = java.util.UUID.randomUUID().toString // See page 59-60, sphere needs to be unique
  var transform: matrix = matrix.identity_matrix

  def intersect(r: ray): intersections = {
    val r2 = r.transform(this.transform.inverse())
    val sphere_to_ray = r2.origin - new point(0,0,0)
    val a = r2.direction dot r2.direction
    val b = 2 * (r2.direction dot sphere_to_ray)
    val c = (sphere_to_ray dot sphere_to_ray) - 1
    val discriminant = math.pow(b,2) - 4*a*c
    if (discriminant < 0) {
      return new intersections()
    }
    val t1 = (-b - math.sqrt(discriminant)) / (2*a)
    val t2 = (-b + math.sqrt(discriminant)) / (2*a)
    new intersections(new intersection(t1,this), new intersection(t2,this))
  }

  def set_transform(t: matrix): Unit = {
    this.transform = t
  }

  override def normal_at(p: point): vector = {
    val object_point = this.transform.inverse() * p
    val object_normal = object_point - new point(0,0,0)
    val world_normal = this.transform.inverse().transpose() * object_normal
    world_normal.w = 0
    world_normal.normalize().to_vector()
  }
}
