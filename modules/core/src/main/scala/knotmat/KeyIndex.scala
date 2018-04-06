package knotmat

abstract class KeyIndex[K, V, M] {

  def fromIndexed(x: ArrayPair[Int, V]): M
  def toIndexed(x: M): ArrayPair[Int, V]

}

object KeyIndex {

  def forArrayPair[K, V](index: Index[K]): KeyIndex[K, V, ArrayPair[K, V]] =
    new ForArrayPair[K, V](index)

  def forMap[K, V](index: Index[K]): KeyIndex[K, V, Map[K, V]] =
    new ForMap[K, V](index)

  final private[this] class ForArrayPair[K, V](index: Index[K]) extends KeyIndex[K, V, ArrayPair[K, V]] {

    def fromIndexed(x: ArrayPair[Int, V]): ArrayPair[K, V] =
      (x._1.map(index.fromIndexed), x._2)

    def toIndexed(x: ArrayPair[K, V]): ArrayPair[Int, V] =
      (x._1.map(index.toIndexed), x._2)

  }

  final private[this] class ForMap[K, V](index: Index[K]) extends KeyIndex[K, V, Map[K, V]] {

    def fromIndexed(x: ArrayPair[Int, V]): Map[K, V] =
      x._1.map(index.fromIndexed).zip(x._2).toMap

    def toIndexed(x: Map[K, V]): ArrayPair[Int, V] = {
      val (ks, vs) = x.toArray.unzip
      (ks.map(index.toIndexed), vs)
    }

  }

}
