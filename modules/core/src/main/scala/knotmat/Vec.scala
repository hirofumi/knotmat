package knotmat

import scala.collection.mutable

sealed abstract class Vec[@specialized(Double, Float, Int, Long) A] {

  def densePart: IndexedSeq[Array[A]]
  def sparsePart: IndexedSeq[Sparse[Int, A]]
  def foreach(f: Vec.Callback[A]): Unit

}

object Vec {

  def apply[A](dense: Array[A], sparse: Sparse[Int, A]*): Vec[A] =
    DenseWithSparse(Vector(dense), sparse.toIndexedSeq)

  def concat[@specialized(Double, Float, Int, Long) A](vs: Vec[A]*): Vec[A] = {
    val dense  = mutable.ArrayBuffer.empty[Array[A]]
    val sparse = mutable.ArrayBuffer.empty[Sparse[Int, A]]
    vs.foreach({
      case _: Empty[A] => // nothing to do
      case Dense(cs)   => dense ++= cs
      case Single(v)   => dense += Array(v)
      case v           => dense ++= v.densePart; sparse ++= v.sparsePart
    })
    DenseWithSparse[A](dense, sparse)
  }

  def concatDense[@specialized(Double, Float, Int, Long) A](vs: DenseLike[A]*): Dense[A] =  {
    val chunks = mutable.ArrayBuffer.empty[Array[A]]
    vs.foreach({
      case _: Empty[A] => // nothing to do
      case Dense(cs)   => chunks ++= cs
      case Single(v)   => chunks += Array(v)
    })
    Dense[A](chunks)
  }

  def concatSingle[A](vs: Single[A]*): Dense[A] =
    Dense(IndexedSeq(vs.map[A, Array[A]](_.value)))

  def dense[A](chunks: Array[A]*): Dense[A] =
    Dense(chunks.toIndexedSeq)

  def single[A](value: A): Single[A] =
    Single(value)

  def sparse[A](sparse: Sparse[Int, A]*): Vec[A] =
    DenseWithSparse(Vector.empty, sparse.toIndexedSeq)

  abstract class Callback[@specialized(Double, Float, Int, Long) -A] {

    def apply(i: Int, a: A): Unit

  }

  sealed abstract class DenseLike[@specialized(Double, Float, Int, Long) A] protected () extends Vec[A] {

    override final def sparsePart: IndexedSeq[Sparse[Int, A]] =
      Vector.empty

  }

  final case class Dense[@specialized(Double, Float, Int, Long) A] private (
    chunks: IndexedSeq[Array[A]]
  ) extends DenseLike[A] {

    override def densePart: IndexedSeq[Array[A]] =
      chunks.toIndexedSeq // TODO

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

  private[this] final case class DenseWithSparse[@specialized(Double, Float, Int, Long) A](
    densePart: IndexedSeq[Array[A]],
    sparsePart: IndexedSeq[Sparse[Int, A]]
  ) extends Vec[A] {

    def foreach(f: Callback[A]): Unit = {
      var i = 0
      var j = 0
      while (j < densePart.length) {
        val chunk = densePart(j)
        var k = 0
        while (k < chunk.length) {
          f(i + k, chunk(k))
          k += 1
        }
        i += k
        j += 1
      }
      j = 0
      while (j < sparsePart.length) {
        val v = sparsePart(j)
        var k = 0
        while (k < v._1.length) {
          f(i + j + sparsePart.length * v._1(k), v._2(k))
          k += 1
        }
        j += 1
      }
    }

  }

  final class Empty[A]() extends DenseLike[A] {

    def densePart: IndexedSeq[Array[A]] =
      Vector.empty

    def foreach(f: Callback[A]): Unit =
      ()

  }

  object Empty extends EmptyInstances {

    implicit val emptyDouble: Empty[Double] = new Empty[Double]()
    implicit val emptyFloat: Empty[Float] = new Empty[Float]()
    implicit val emptyInt: Empty[Int] = new Empty[Int]()
    implicit val emptyLong: Empty[Long] = new Empty[Long]()

    def apply[A: Empty]: Empty[A] =
      implicitly[Empty[A]]

  }

  trait EmptyInstances {

    implicit def newEmpty[A]: Empty[A] =
      new Empty()

  }

  final case class Single[@specialized(Double, Float, Int, Long) A] private (
    value: A
  ) extends DenseLike[A] {

    def densePart: IndexedSeq[Array[A]] =
      Vector(Array(value))

    def foreach(f: Callback[A]): Unit = {
      f(0, value)
      ()
    }

  }

}
