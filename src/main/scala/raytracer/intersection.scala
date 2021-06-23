package raytracer

class intersection(val t: Double, val `object`: scene_object) {
  def prepare_computations(r: ray): computations = {
    val comps = new computations()
    comps.t = Some(this.t)
    comps.`object` = Some(this.`object`)
    comps.point = Some(r.position(comps.t.get))
    comps.eyev = Some((-r.direction).to_vector())
    comps.normalv = Some(comps.`object`.get.normal_at(comps.point.get))
    if ((comps.normalv.get dot comps.eyev.get) < 0) {
      comps.inside = Some(true)
      comps.normalv = Some((-comps.normalv.get).to_vector())
    } else {
      comps.inside = Some(false)
    }
    comps
  }
}

class intersections(intersections: intersection*) {
  private var intersects: Seq[intersection] = intersections
  var length: Int = this.intersects.length
  var size: Int = length

  def add(i: intersection): Unit = {
    this.intersects = this.intersects :+ i
    this.length = this.intersects.length
    this.size = this.length
  }

  def apply(i: Int): intersection = {
    this.intersects(i)
  }

  def hit(): Option[intersection] = {
    this.intersects.filter(_.t >= 0).minByOption(_.t)
  }
}