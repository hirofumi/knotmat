package knotmat

import scala.collection.mutable

sealed abstract class DataCodec[
@specialized(Double, Float, Int, Long) A1,
@specialized(Double, Float, Int, Long) A2
] { self =>

  def dim: Int

  def encode(a1: A1): A2
  def decode(a2: A2): A1

  final def +[A3](that: DataCodec[A2, A3]): DataCodec[A1, A3] =
    new DataCodec {
      final def dim: Int = that.dim
      final def encode(a1: A1): A3 = that.encode(self.encode(a1))
      final def decode(a3: A3): A1 = self.decode(that.decode(a3))
    }

  final def sparse: DataCodec[Array[A1], Array[A2]] =
    new DataCodec {
      final def dim: Int = self.dim
      final def encode(a1: Array[A1]): Array[A2] = a1.map(self.encode)
      final def decode(a2: Array[A2]): Array[A1] = a2.map(self.decode)
    }

}

object DataCodec {

  implicit final class DataCodecArraySyntax[A1, A2](private val self: DataCodec[Array[A1], Array[A2]]) extends AnyVal {

    def weighted[@specialized(Double, Float, Int, Long) B]: DataCodec[ArrayPair[A1, B], ArrayPair[A2, B]] =
      new DataCodec {
        final def dim: Int =
          self.dim
        final def encode(pair: ArrayPair[A1, B]): ArrayPair[A2, B] =
          (self.encode(pair._1), pair._2)
        final def decode(pair: ArrayPair[A2, B]): ArrayPair[A1, B] =
          (self.decode(pair._1), pair._2)
      }

  }

  final class Index[A <: AnyRef] private () extends DataCodec[A, Int] {

    private[this] val fromInt: mutable.ArrayBuffer[A] =
      mutable.ArrayBuffer.empty

    private[this] val toInt: mutable.AnyRefMap[A, Int] =
      mutable.AnyRefMap.empty

    def dim: Int =
      fromInt.length

    def decode(i: Int): A =
      fromInt(i)

    def encode(a: A): Int =
      toInt.getOrElseUpdate(a, { fromInt += a; fromInt.length - 1 })

    def toIndexedSeq: IndexedSeq[A] =
      fromInt

    def toMap: collection.Map[A, Int] =
      toInt

  }

  def bow: DataCodec[ArrayPair[String, Int], ArrayPair[Int, Int]] =
    bow(index)

  def bow(index: Index[String]): DataCodec[ArrayPair[String, Int], ArrayPair[Int, Int]] =
    weightedWords[Int](index)

  def index[A <: AnyRef]: Index[A] =
    new Index[A]

  def weightedWords[@specialized(Double, Float, Int, Long) A]: DataCodec[ArrayPair[String, A], ArrayPair[Int, A]] =
    weightedWords(index)

  def weightedWords[@specialized(Double, Float, Int, Long) A](index: Index[String]): DataCodec[ArrayPair[String, A], ArrayPair[Int, A]] =
    index.sparse.weighted[A]

}
