package knotmat

import scala.collection.mutable

sealed abstract class Index[A] {

  def dim: Int
  def fromIndexed(i: Int): A
  def toIndexed(a: A): Int
  def toIndexedSeq: IndexedSeq[A]
  def toMap: collection.Map[A, Int]

}

object Index {

  final class NoLimit[A <: AnyRef] extends Index[A] {

    private[this] val fromInt: mutable.ArrayBuffer[A] =
      mutable.ArrayBuffer.empty

    private[this] val toInt: mutable.AnyRefMap[A, Int] =
      mutable.AnyRefMap.empty

    def dim: Int =
      fromInt.length

    def fromIndexed(i: Int): A =
      fromInt(i)

    def toIndexed(a: A): Int =
      toInt.getOrElseUpdate(a, {
        fromInt += a; fromInt.length - 1
      })

    def toIndexedSeq: IndexedSeq[A] =
      fromInt

    def toMap: collection.Map[A, Int] =
      toInt

  }

}
