package knotmat

sealed abstract class PreVector[@specialized(Double, Float, Int, Long) +A] {

  def ++[A2 >: A](that: PreVector.Sparse[A2]): PreVector.Sparse[A2] =
    ???

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
    chunks: Seq[Array[A]]
  ) extends PreVector[A] {

    def ++(that: Dense[A]): Dense[A] =
      Dense(chunks ++ that.chunks)

    def ++(that: Single[A]): Dense[A] =
      Dense(chunks :+ Array(that.value))

    def dim: Int =
      chunks.foldLeft(0)(_ + _.length)

    def foreach(f: Callback[A]): Unit =
      for (chunk <- chunks) {
        var i = 0
        while (i < chunk.length) {
          f(i, chunk(i))
          i += 1
        }
      }

  }

  final case class Single[@specialized(Double, Float, Int, Long) A](
    value: A
  ) extends PreVector[A] {

    def ++(that: Dense[A]): Dense[A] =
      Dense(Array(value) +: that.chunks)

    def ++(that: Single[A]): Dense[A] =
      Dense(Seq(Array(value, that.value)))

    def dim: Int =
      1

    def foreach(f: Callback[A]): Unit = {
      f(0, value)
      ()
    }

  }

  final case class Sparse[@specialized(Double, Float, Int, Long) A](
    dense: IndexedSeq[Array[A]],
    sparse: IndexedSeq[ArrayPair[Int, A]]
  ) extends PreVector[A] {

    def foreach(f: Callback[A]): Unit = {
      var i = 0
      var j = 0
      while (j < dense.length) {
        val chunk = dense(j)
        var k = 0
        while (k < chunk.length) {
          f(i + k, chunk(k))
          k += 1
        }
        i += k
        j += 1
      }
      j = 0
      while (j < sparse.length) {
        val v = sparse(j)
        var k = 0
        while (k < v._1.length) {
          f(i + j + sparse.length * v._1(k), v._2(k))
          k += 1
        }
        j += 1
      }
    }

  }

}
