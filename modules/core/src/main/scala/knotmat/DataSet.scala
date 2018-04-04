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

      def fromIterable[A](dim: => Int): Iterable[Point.Dense.NoLabel[A]] => NoLabel[A] =
        points =>
          new NoLabel[A] {
            def foreach[U](f: Point.Dense.NoLabel[A] => U): (Int, Int) = {
              var i = 0
              for (point <- points) {
                f(point)
                i += 1
              }
              (i, dim)
            }
          }

    }

  }

}
