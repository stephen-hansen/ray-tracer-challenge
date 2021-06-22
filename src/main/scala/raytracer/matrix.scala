package raytracer

class matrix(val contents: Array[Array[Double]]) {
  def apply(i: Int, j: Int): Double = {
    this.contents(i)(j)
  }

  def update(i: Int, j: Int, item: Double) : Unit = {
    this.contents(i)(j) = item
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case m: matrix =>
        if (this.contents.length != m.contents.length) {
          return false
        }
        for (i <- this.contents.indices) {
          if (this.contents(i).length != m.contents(i).length) {
            return false
          }
          for (j <- this.contents(i).indices) {
            if (!utils.float_equals(this.contents(i)(j), m.contents(i)(j))) {
              return false
            }
          }
        }
        true
      case _ => false
    }
  }

  def *(that: matrix): matrix = {
    val rows = this.contents.length
    val cols = this.contents(0).length
    val M = new matrix(Array.ofDim[Double](rows, cols))
    for (row <- this.contents.indices) {
      for (col <- this.contents(row).indices) {
        M(row,col) = 0
        for (k <- 0 until rows) {
          M(row,col) += this(row, k) * that(k, col)
        }
      }
    }
    M
  }

  def *(that: tuple): tuple = {
    val rows = this.contents.length
    val t = new tuple(0, 0, 0, 0)
    for (row <- this.contents.indices) {
      for (k <- 0 until rows) {
        t(row) += this(row, k) * that(k)
      }
    }
    t
  }

  def transpose(): matrix = {
    val rows = this.contents.length
    val cols = this.contents(0).length
    val M = new matrix(Array.ofDim[Double](rows, cols))
    for (row <- this.contents.indices) {
      for (col <- this.contents(row).indices) {
        M(col, row) = this(row, col)
      }
    }
    M
  }

  def determinant(): Double = {
    if (this.contents.length == 2 && this.contents(0).length == 2) {
      // 2x2 base case
      this (0, 0) * this (1, 1) - this (0, 1) * this (1, 0)
    } else {
      // recursive for 3x3 and 4x4
      var result = 0.0
      for (j <- this.contents(0).indices) {
        result += this(0,j)*this.cofactor(0,j)
      }
      result
    }
  }

  def submatrix(row: Int, column: Int): matrix = {
    val rows = this.contents.length
    val cols = this.contents(0).length
    val M = new matrix(Array.ofDim[Double](rows-1, cols-1))
    for (r <- this.contents.indices) {
      if (r != row) {
        for (c <- this.contents(r).indices) {
          if (c != column) {
            val new_r = if (r > row) r-1 else r
            val new_c = if (c > column) c-1 else c
            M(new_r, new_c) = this(r, c)
          }
        }
      }
    }
    M
  }

  def minor(row: Int, column: Int): Double = {
    this.submatrix(row, column).determinant()
  }

  def cofactor(row: Int, column: Int): Double = {
    val factor = if ((row+column)%2 == 1) -1 else 1
    factor * minor(row, column)
  }

  def is_invertible(): Boolean = {
    !utils.float_equals(this.determinant(), 0)
  }

  def inverse(): matrix = {
    if (!this.is_invertible()) {
      throw new ArithmeticException("trying to invert a non-invertible matrix")
    }
    val rows = this.contents.length
    val cols = this.contents(0).length
    val M2 = new matrix(Array.ofDim[Double](rows, cols))
    for (row <- 0 until rows) {
      for (col <- 0 until cols) {
        val c = this.cofactor(row, col)
        M2(col, row) = c / this.determinant()
      }
    }
    M2
  }
}

object matrix {
  val identity_matrix : matrix = new matrix(
    Array(
      Array(1,0,0,0),
      Array(0,1,0,0),
      Array(0,0,1,0),
      Array(0,0,0,1),
    )
  )
}
