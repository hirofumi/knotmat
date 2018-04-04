package knotmat

sealed abstract class PreVector[@specialized(Double, Float, Int, Long) +A] {

  def foreach(f: PreVector.Callback[A]): Unit

}

object PreVector {

  abstract class Callback[@specialized(Double, Float, Int, Long) -A] {
    def apply(i: Int, a: A): Unit
  }

  final case object Empty extends PreVector[Nothing] {

    def dim: Int =
      0

    def foreach(f: Callback[Nothing]): Unit =
      ()

  }

  final case class Dense[@specialized(Double, Float, Int, Long) A](
    values: Array[A]
  ) extends PreVector[A] {

    def dim: Int =
      values.length

    def foreach(f: Callback[A]): Unit = {
      var i = 0
      while (i < values.length) {
        f(i, values(i))
        i += 1
      }
    }

  }

  final case class Single[@specialized(Double, Float, Int, Long) +A](
    value: A
  ) extends PreVector[A] {

    def dim: Int =
      1

    def foreach(f: Callback[A]): Unit = {
      f(0, value)
      ()
    }

  }

  final case class Sparse[@specialized(Double, Float, Int, Long) A](
    dense: Array[A],
    sparse: Seq[ArrayPair[Int, A]]
  ) extends PreVector[A] {

    def foreach(f: Callback[A]): Unit = {
      var i = 0
      while (i < dense.length) {
        f(i, dense(i))
        i += 1
      }
      var j = 0
      while (j < sparse.length) {
        val s = sparse(j)
        var k = 0
        while (k < s._1.length) {
          f(i + j + sparse.length * s._1(k), s._2(k))
          k += 1
        }
        j += 1
      }
    }

  }

}
