package knotmat

abstract class DataSet[+S] {

  def foreach[U](f: S => U): (Int, Int)

}

object DataSet {

  object Dense {

    type MultiLabel[A]  = DataSet[Point.Dense.MultiLabel[A]]
    type NoLabel[A]     = DataSet[Point.Dense.NoLabel[A]]
    type SingleLabel[A] = DataSet[Point.Dense.SingleLabel[A]]

    object NoLabel {

      /*
      def fromIterable[A, B](dim: => Int)(implicit E: PointEncoder.Dense.NoLabel[A, B]): Iterable[A] => NoLabel[B] =
        points =>
          new NoLabel[B] {
            def foreach[U](f: Point.Dense.NoLabel[B] => U): (Int, Int) = {
              var i = 0
              for (point <- points) {
                f(E.encode(point))
                i += 1
              }
              (i, dim)
            }
          }
          */

    }

  }

}
