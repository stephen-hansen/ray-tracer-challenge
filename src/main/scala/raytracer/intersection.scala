package raytracer

class intersection(val t: Double, val `object`: scene_object) {
}

class intersections(intersections: intersection*) {
  private val intersects : Seq[intersection] = intersections
  val length: Int = intersects.length
  val size: Int = length

  def apply(i: Int): intersection = {
    intersects(i)
  }

  def hit(): Option[intersection] = {
    intersects.filter(_.t >= 0).minByOption(_.t)
  }
}