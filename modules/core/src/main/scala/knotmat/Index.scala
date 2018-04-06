package knotmat

import scala.collection.mutable

sealed abstract class Index[A] {

  def assign(a: A): Int
  def assignMap[B](map: Map[A, B]): Sparse[Int, B]
  def assignSparse[B](sparse: Sparse[A, B]): Sparse[Int, B]
  def clear(): Unit
  def dim: Int
  def lookup(i: Int): A
  def lookupMap[B](indexed: Sparse[Int, B]): Map[A, B]
  def lookupSparse[B](indexed: Sparse[Int, B]): Sparse[A, B]
  def toIndexedSeq: IndexedSeq[A]
  def toMap: collection.Map[A, Int]

}

object Index {

  val string: Index[String] = new Default

  final class Default[A <: AnyRef] extends Index[A] {

    private[this] val fromInt: mutable.ArrayBuffer[A] =
      mutable.ArrayBuffer.empty

    private[this] val toInt: mutable.AnyRefMap[A, Int] =
      mutable.AnyRefMap.empty

    def assign(a: A): Int =
      toInt.getOrElseUpdate(a, {
        fromInt += a; fromInt.length - 1
      })

    def assignMap[B](kvs: Map[A, B]): Sparse[Int, B] = {
      val (ks, vs) = kvs.toArray.unzip
      (ks.map(assign), vs)
    }

    def assignSparse[B](sparse: Sparse[A, B]): Sparse[Int, B] =
      (sparse._1.map(assign), sparse._2)

    def clear(): Unit = {
      fromInt.clear()
      toInt.clear()
    }

    def dim: Int =
      fromInt.length

    def lookup(i: Int): A =
      fromInt(i)

    def lookupMap[B](indexed: Sparse[Int, B]): Map[A, B] =
      indexed._1.map(lookup).zip(indexed._2).toMap

    def lookupSparse[B](indexed: Sparse[Int, B]): Sparse[A, B] =
      (indexed._1.map(lookup), indexed._2)

    def toIndexedSeq: IndexedSeq[A] =
      fromInt

    def toMap: collection.Map[A, Int] =
      toInt

  }

}
