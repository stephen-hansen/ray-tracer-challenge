package raytracer

import org.scalatest.freespec.AnyFreeSpec

class world_spec extends AnyFreeSpec {
  "A world" - {
    "is empty by default" in {
      val w = new world()
      assert(w.length == 0)
      assert(w.light.isEmpty)
    }
    "has correct default constructor" in {
      val light = new point_light(new point(-10,10,-10), new color(1,1,1))
      val s1 = new sphere()
      s1.material.color = new color(0.8,1.0,0.6)
      s1.material.diffuse = 0.7
      s1.material.specular = 0.2
      val s2 = new sphere()
      s2.set_transform(new scaling(0.5,0.5,0.5))
      val w = world.default_world()
      assert(w.light.isDefined)
      assert(w.light.get == light)
      assert(w.contains(s1))
      assert(w.contains(s2))
    }
    "intersects a ray correctly" in {
      val w = world.default_world()
      val r = new ray(new point(0,0,-5),new vector(0,0,1))
      val xs = w.intersect_world(r)
      assert(xs.length == 4)
      assert(utils.float_equals(xs(0).t,4))
      assert(utils.float_equals(xs(1).t,4.5))
      assert(utils.float_equals(xs(2).t,5.5))
      assert(utils.float_equals(xs(3).t,6))
    }
    "shades an intersection" in {
      val w = world.default_world()
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val shape = w(0)
      val i = new intersection(4, shape)
      val comps = i.prepare_computations(r)
      val c = w.shade_hit(comps)
      assert(c == new color(0.38066, 0.47583, 0.2855))
    }
    "shades an intersection from inside" in {
      val w = world.default_world()
      w.light = Some(new point_light(new point(0,0.25,0), new color(1,1,1)))
      val r = new ray(new point(0,0,0), new vector(0,0,1))
      val shape = w(1)
      val i = new intersection(0.5, shape)
      val comps = i.prepare_computations(r)
      val c = w.shade_hit(comps)
      assert(c == new color(0.90498, 0.90498, 0.90498))
    }
    "gets color when a ray misses" in {
      val w = world.default_world()
      val r = new ray(new point(0,0,-5), new vector(0,1,0))
      val c = w.color_at(r)
      assert(c == new color(0,0,0))
    }
    "gets color when a ray hits" in {
      val w = world.default_world()
      val r = new ray(new point(0,0,-5), new vector(0,0,1))
      val c = w.color_at(r)
      assert(c == new color(0.38066, 0.47583, 0.2855))
    }
    "gets color with an intersection behind a ray" in {
      val w = world.default_world()
      val outer = w(0)
      outer.material.ambient = 1
      val inner = w(1)
      inner.material.ambient = 1
      val r = new ray(new point(0,0,0.75), new vector(0,0,-1))
      val c = w.color_at(r)
      assert(c == inner.material.color)
    }
    "detects no shadow with object colinear to point and light" in {
      val w = world.default_world()
      val p = new point(0,10,0)
      assert(!w.is_shadowed(p))
    }
    "detects shadow with object between point and light" in {
      val w = world.default_world()
      val p = new point(10,-10,10)
      assert(w.is_shadowed(p))
    }
    "detects no shadow with object behind light" in {
      val w = world.default_world()
      val p = new point(-20,20,-20)
      assert(!w.is_shadowed(p))
    }
    "detects no shadow with object behind point" in {
      val w = world.default_world()
      val p = new point(-2,2,-2)
      assert(!w.is_shadowed(p))
    }
    "gives an intersection in shadow" in {
      val w = new world()
      w.light = Some(new point_light(new point(0,0,-10), new color(1,1,1)))
      val s1 = new sphere()
      w.add_object(s1)
      val s2 = new sphere()
      s2.set_transform(new translation(0,0,10))
      w.add_object(s2)
      val r = new ray(new point(0,0,5), new vector(0,0,1))
      val i = new intersection(4,s2)
      val comps = i.prepare_computations(r)
      val c = w.shade_hit(comps)
      assert(c == new color(0.1,0.1,0.1))
    }
  }
}
