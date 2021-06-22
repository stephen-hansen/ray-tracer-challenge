package raytracer

import org.scalatest.freespec.AnyFreeSpec

class matrix_spec extends AnyFreeSpec {
  "A matrix" - {
    "can be 4x4" in {
      val M = new matrix(
        Array(
          Array(1, 2, 3, 4),
          Array(5.5, 6.5, 7.5, 8.5),
          Array(9, 10, 11, 12),
          Array(13.5, 14.5, 15.5, 16.5)
        )
      )
      assert(utils.float_equals(M(0,0), 1))
      assert(utils.float_equals(M(0,3), 4))
      assert(utils.float_equals(M(1,0), 5.5))
      assert(utils.float_equals(M(1,2), 7.5))
      assert(utils.float_equals(M(2,2), 11))
      assert(utils.float_equals(M(3,0), 13.5))
      assert(utils.float_equals(M(3,2), 15.5))
    }
    "can be 2x2" in {
      val M = new matrix(
        Array(
          Array(-3, 5),
          Array(1, -2)
        )
      )
      assert(utils.float_equals(M(0,0), -3))
      assert(utils.float_equals(M(0,1), 5))
      assert(utils.float_equals(M(1,0), 1))
      assert(utils.float_equals(M(1,1), -2))
    }
    "can be 3x3" in {
      val M = new matrix(
        Array(
          Array(-3, 5, 0),
          Array(1, -2, -7),
          Array(0, 1, 1)
        )
      )
      assert(utils.float_equals(M(0,0), -3))
      assert(utils.float_equals(M(1,1), -2))
      assert(utils.float_equals(M(2,2), 1))
    }
    "can be compared for equality" in {
      val A = new matrix(
        Array(
          Array(1,2,3,4),
          Array(5,6,7,8),
          Array(9,8,7,6),
          Array(5,4,3,2),
        )
      )
      val B = new matrix(
        Array(
          Array(1,2,3,4),
          Array(5,6,7,8),
          Array(9,8,7,6),
          Array(5,4,3,2),
        )
      )
      assert(A == B)
    }
    "can be compared for inequality" in {
      val A = new matrix(
        Array(
          Array(1,2,3,4),
          Array(5,6,7,8),
          Array(9,8,7,6),
          Array(5,4,3,2),
        )
      )
      val B = new matrix(
        Array(
          Array(2,3,4,5),
          Array(6,7,8,9),
          Array(8,7,6,5),
          Array(4,3,2,1),
        )
      )
      assert(A != B)
    }
    "multiplied with a matrix produces a matrix" in {
      val A = new matrix(
        Array(
          Array(1,2,3,4),
          Array(5,6,7,8),
          Array(9,8,7,6),
          Array(5,4,3,2),
        )
      )
      val B = new matrix(
        Array(
          Array(-2,1,2,3),
          Array(3,2,1,-1),
          Array(4,3,6,5),
          Array(1,2,7,8),
        )
      )
      assert(A * B == new matrix(
        Array(
          Array(20,22,50,48),
          Array(44,54,114,108),
          Array(40,58,110,102),
          Array(16,26,46,42),
        )
      ))
    }
    "multiplied with a tuple produces a tuple" in {
      val A = new matrix(
        Array(
          Array(1,2,3,4),
          Array(2,4,4,2),
          Array(8,6,4,1),
          Array(0,0,0,1),
        )
      )
      val b = new tuple(1, 2, 3, 1)
      assert(A * b == new tuple(18, 24, 33, 1))
    }
    "multiplied by the identity matrix does not change the matrix" in {
      val A = new matrix(
        Array(
          Array(0,1,2,4),
          Array(1,2,4,8),
          Array(2,4,8,16),
          Array(4,8,16,32),
        )
      )
      assert(A * matrix.identity_matrix == A)
    }
    "as identity multiplied by tuple does not change the tuple" in {
      val a = new tuple(1,2,3,4)
      assert(matrix.identity_matrix * a == a)
    }
    "transposed yields a proper matrix" in {
      val A = new matrix(
        Array(
          Array(0,9,3,0),
          Array(9,8,0,8),
          Array(1,8,5,3),
          Array(0,0,5,8),
        )
      )
      assert(A.transpose() == new matrix(
        Array(
          Array(0,9,1,0),
          Array(9,8,8,0),
          Array(3,0,5,5),
          Array(0,8,3,8),
        )
      ))
    }
    "as identity transposed does not change the matrix" in {
      val A = matrix.identity_matrix.transpose()
      assert(A == matrix.identity_matrix)
    }
    "calculates 2x2 determinant" in {
      val A = new matrix(
        Array(
          Array(1,5),
          Array(-3,2),
        )
      )
      assert(utils.float_equals(A.determinant(), 17))
    }
    "gets 2x2 submatrix from 3x3" in {
      val A = new matrix(
        Array(
          Array(1,5,0),
          Array(-3,2,7),
          Array(0,6,-3),
        )
      )
      assert(A.submatrix(0,2) == new matrix(
        Array(
          Array(-3,2),
          Array(0,6),
        )
      ))
    }
    "gets 3x3 submatrix from 4x4" in {
      val A = new matrix(
        Array(
          Array(-6,1,1,6),
          Array(-8,5,8,6),
          Array(-1,0,8,2),
          Array(-7,1,-1,1),
        )
      )
      assert(A.submatrix(2,1) == new matrix(
        Array(
          Array(-6,1,6),
          Array(-8,8,6),
          Array(-7,-1,1),
        )
      ))
    }
    "computes the minor as the determinant of the submatrix" in {
      val A = new matrix(
        Array(
          Array(3,5,0),
          Array(2,-1,-7),
          Array(6,-1,5),
        )
      )
      val B = A.submatrix(1,0)
      assert(utils.float_equals(B.determinant(), 25))
      assert(utils.float_equals(A.minor(1,0), 25))
    }
    "computes the cofactor of a 3x3 matrix" in {
      val A = new matrix(
        Array(
          Array(3,5,0),
          Array(2,-1,-7),
          Array(6,-1,5),
        )
      )
      assert(utils.float_equals(A.minor(0,0),-12))
      assert(utils.float_equals(A.cofactor(0,0),-12))
      assert(utils.float_equals(A.minor(1,0),25))
      assert(utils.float_equals(A.cofactor(1,0),-25))
    }
    "computes the determinant of a 3x3 matrix" in {
      val A = new matrix(
        Array(
          Array(1,2,6),
          Array(-5,8,-4),
          Array(2,6,4),
        )
      )
      assert(utils.float_equals(A.cofactor(0,0),56))
      assert(utils.float_equals(A.cofactor(0,1),12))
      assert(utils.float_equals(A.cofactor(0,2),-46))
      assert(utils.float_equals(A.determinant(),-196))
    }
    "computes the determinant of a 4x4 matrix" in {
      val A = new matrix(
        Array(
          Array(-2,-8,3,5),
          Array(-3,1,7,3),
          Array(1,2,-9,6),
          Array(-6,7,7,-9),
        )
      )
      assert(utils.float_equals(A.cofactor(0,0),690))
      assert(utils.float_equals(A.cofactor(0,1),447))
      assert(utils.float_equals(A.cofactor(0,2),210))
      assert(utils.float_equals(A.cofactor(0,3),51))
      assert(utils.float_equals(A.determinant(),-4071))
    }
    "can test for invertible" in {
      val A = new matrix(
        Array(
          Array(6,4,4,4),
          Array(5,5,7,6),
          Array(4,-9,3,-7),
          Array(9,1,7,-6),
        )
      )
      assert(utils.float_equals(A.determinant(), -2120))
      assert(A.is_invertible())
    }
    "can test for not invertible" in {
      val A = new matrix(
        Array(
          Array(-4,2,-2,-3),
          Array(9,6,2,6),
          Array(0,-5,1,-5),
          Array(0,0,0,0),
        )
      )
      assert(utils.float_equals(A.determinant(), 0))
      assert(!A.is_invertible())
    }
    "can calculate the inverse" in {
      val A = new matrix(
        Array(
          Array(-5,2,6,-8),
          Array(1,-5,1,8),
          Array(7,7,-6,-7),
          Array(1,-3,7,4),
        )
      )
      val B = A.inverse()
      assert(utils.float_equals(A.determinant(),532))
      assert(utils.float_equals(A.cofactor(2,3),-160))
      assert(utils.float_equals(B(3,2),-160.0/532.0))
      assert(utils.float_equals(A.cofactor(3,2),105))
      assert(utils.float_equals(B(2,3),105.0/532.0))
      assert(B == new matrix(
        Array(
          Array(0.21805,0.45113,0.24060,-0.04511),
          Array(-0.80827,-1.45677,-0.44361,0.52068),
          Array(-0.07895,-0.22368,-0.05263,0.19737),
          Array(-0.52256,-0.81391,-0.30075,0.30639),
        )
      ))
    }
    "multiplying by inverse yields original matrix" in {
      val A = new matrix(
        Array(
          Array(3,-9,7,3),
          Array(3,-8,2,-9),
          Array(-4,4,4,1),
          Array(-6,5,-1,1),
        )
      )
      val B = new matrix(
        Array(
          Array(8,2,2,2),
          Array(3,-1,7,0),
          Array(7,0,5,4),
          Array(6,-2,0,5),
        )
      )
      val C = A * B
      assert(C * B.inverse() == A)
    }
  }
  "A translation" - {
    "can move a point" in {
      val transform = new translation(5,-3,2)
      val p = new point(-3,4,5)
      assert(transform * p == new point(2,1,7))
    }
    "can move a point in reverse" in {
      val transform = new translation(5,-3,2)
      val inv = transform.inverse()
      val p = new point(-3,4,5)
      assert(inv * p == new point(-8,7,3))
    }
    "does not affect vectors" in {
      val transform = new translation(5,-3,2)
      val v = new vector(-3,4,5)
      assert(transform * v == v)
    }
  }
  "A scaling" - {
    "can move a point" in {
      val transform = new scaling(2,3,4)
      val p = new point(-4,6,8)
      assert(transform * p == new point(-8,18,32))
    }
    "can resize a vector" in {
      val transform = new scaling(2,3,4)
      val v = new vector(-4,6,8)
      assert(transform * v == new vector(-8,18,32))
    }
    "can scale a vector in reverse" in {
      val transform = new scaling(2,3,4)
      val inv = transform.inverse()
      val v = new vector(-4,6,8)
      assert(inv * v == new vector(-2,2,2))
    }
    "can reflect a point" in {
      val transform = new scaling(-1,1,1)
      val p = new point(2,3,4)
      assert(transform * p == new point(-2,3,4))
    }
  }
  "A rotation_x" - {
    "can rotate a point around the x axis" in {
      val p = new point(0,1,0)
      val half_quarter = new rotation_x(math.Pi/4)
      val full_quarter = new rotation_x(math.Pi/2)
      assert(half_quarter * p == new point(0, math.sqrt(2)/2, math.sqrt(2)/2))
      assert(full_quarter * p == new point(0,0,1))
    }
    "can rotate in the opposite direction" in {
      val p = new point(0,1,0)
      val half_quarter = new rotation_x(math.Pi/4)
      val inv = half_quarter.inverse()
      assert(inv * p == new point(0, math.sqrt(2)/2, -math.sqrt(2)/2))
    }
  }
  "A rotation_y" - {
    "can rotate a point around the y axis" in {
      val p = new point(0,0,1)
      val half_quarter = new rotation_y(math.Pi/4)
      val full_quarter = new rotation_y(math.Pi/2)
      assert(half_quarter * p == new point(math.sqrt(2)/2, 0, math.sqrt(2)/2))
      assert(full_quarter * p == new point(1,0,0))
    }
  }
  "A rotation_z" - {
    "can rotate a point around the z axis" in {
      val p = new point(0,1,0)
      val half_quarter = new rotation_z(math.Pi/4)
      val full_quarter = new rotation_z(math.Pi/2)
      assert(half_quarter * p == new point(-math.sqrt(2)/2, math.sqrt(2)/2, 0))
      assert(full_quarter * p == new point(-1,0,0))
    }
  }
  "A shearing" - {
    "moves x in proportion to y" in {
      val transform = new shearing(1,0,0,0,0,0)
      val p = new point(2,3,4)
      assert(transform * p == new point(5,3,4))
    }
    "moves x in proportion to z" in {
      val transform = new shearing(0,1,0,0,0,0)
      val p = new point(2,3,4)
      assert(transform * p == new point(6,3,4))
    }
    "moves y in proportion to x" in {
      val transform = new shearing(0,0,1,0,0,0)
      val p = new point(2,3,4)
      assert(transform * p == new point(2,5,4))
    }
    "moves y in proportion to z" in {
      val transform = new shearing(0,0,0,1,0,0)
      val p = new point(2,3,4)
      assert(transform * p == new point(2,7,4))
    }
    "moves z in proportion to x" in {
      val transform = new shearing(0,0,0,0,1,0)
      val p = new point(2,3,4)
      assert(transform * p == new point(2,3,6))
    }
    "moves z in proportion to y" in {
      val transform = new shearing(0,0,0,0,0,1)
      val p = new point(2,3,4)
      assert(transform * p == new point(2,3,7))
    }
  }
  "All transformations" - {
    "can be applied in sequence" in {
      val p = new point(1,0,1)
      val A = new rotation_x(math.Pi/2)
      val B = new scaling(5,5,5)
      val C = new translation(10,5,7)
      val p2 = A * p
      assert(p2 == new point(1,-1,0))
      val p3 = B * p2
      assert(p3 == new point(5,-5,0))
      val p4 = C * p3
      assert(p4 == new point(15,0,7))
    }
    "can be chained in reverse order" in {
      val p = new point(1,0,1)
      val A = new rotation_x(math.Pi/2)
      val B = new scaling(5,5,5)
      val C = new translation(10,5,7)
      val T = C * B * A
      assert(T * p == new point(15,0,7))
    }
  }
}
