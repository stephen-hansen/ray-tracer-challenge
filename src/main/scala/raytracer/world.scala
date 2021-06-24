package raytracer

class world {
  var light : Option[point_light] = None
  private var objects : Vector[shape] = Vector()
  val length: Int = objects.length
  val size: Int = length
  def apply(i: Int): shape = {
    objects(i)
  }
  def add_object(s: shape): Unit = {
    objects = objects :+ s
  }
  def contains(s: shape): Boolean = {
    objects.contains(s)
  }
  def intersect_world(r: ray): intersections = {
    var intersects_list : Seq[intersection] = Seq()
    for (obj <- objects) {
      val intersects = obj.intersect(r)
      for (i <- 0 until intersects.length) {
        intersects_list = intersects_list :+ intersects(i)
      }
    }
    intersects_list = intersects_list.sortBy(_.t)
    val final_intersections = new intersections()
    for (inter <- intersects_list) {
      final_intersections.add(inter)
    }
    final_intersections
  }
  def shade_hit(comps: computations): color = {
    val shadowed = this.is_shadowed(comps.over_point.get)
    comps.`object`.get.material.lighting(comps.`object`.get, this.light.get, comps.over_point.get, comps.eyev.get, comps.normalv.get, shadowed)
  }
  def color_at(r: ray): color = {
    val intersects = this.intersect_world(r)
    val hit = intersects.hit()
    if (hit.isEmpty) {
      return new color(0,0,0)
    }
    val comps = hit.get.prepare_computations(r)
    this.shade_hit(comps)
  }
  def is_shadowed(point: point): Boolean = {
    val v = this.light.get.position - point
    val distance = v.magnitude()
    val direction = v.normalize()

    val r = new ray(point, direction.to_vector())
    val intersections = this.intersect_world(r)

    val h = intersections.hit()
    h.isDefined && h.get.t < distance
  }
}

object world {
  def default_world() : world = {
    val light = new point_light(new point(-10,10,-10), new color(1,1,1))
    val s1 = new sphere()
    s1.material.color = new color(0.8,1.0,0.6)
    s1.material.diffuse = 0.7
    s1.material.specular = 0.2
    val s2 = new sphere()
    s2.set_transform(new scaling(0.5,0.5,0.5))
    val w = new world()
    w.light = Some(light)
    w.add_object(s1)
    w.add_object(s2)
    w
  }
}
