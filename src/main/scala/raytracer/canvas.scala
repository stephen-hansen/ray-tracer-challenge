package raytracer

class canvas(val width: Int, val height: Int) {
  val pixels : Array[Array[color]] = {
    val grid = Array.ofDim[color](height, width)
    for (i <- grid.indices) {
      for (j <- grid(i).indices) {
        grid(i)(j) = new color(0, 0, 0)
      }
    }
    grid
  }

  def write_pixel(x: Int, y: Int, c: color): Unit = {
    pixels(y)(x) = c
  }

  def pixel_at(x: Int, y: Int): color = {
    pixels(y)(x)
  }

  def to_ppm(): String = {
    var out = "P3\n"
    out += width + " " + height + "\n"
    out += "255\n"
    var curr_line_char_count = 0
    // Over each row
    for (i <- pixels.indices) {
      var line = ""
      var next = ""
      // Over each column
      for (j <- pixels(i).indices) {
        val c = this.pixel_at(j, i)
        val red = (255 * math.max(math.min(1, c.red), 0)).round
        // Not the first pixel in the line
        if (j != 0) {
          next = " " + red
        } else {
          next = red.toString
        }
        if ((line + next).length >= 70) {
          out += line + "\n"
          line = red.toString
        } else {
          line += next
        }
        val green = (255 * math.max(math.min(1, c.green), 0)).round
        next = " " + green
        if ((line + next).length >= 70) {
          out += line + "\n"
          line = green.toString
        } else {
          line += next
        }
        val blue = (255 * math.max(math.min(1, c.blue), 0)).round
        next = " " + blue
        if ((line + next).length >= 70) {
          out += line + "\n"
          line = blue.toString
        } else {
          line += next
        }
      }
      out += line + "\n"
    }
    out
  }
}
