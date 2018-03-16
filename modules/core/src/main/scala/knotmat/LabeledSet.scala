package knotmat

trait LabeledSet[+F, +L] {

  def foreach[U](f: (F, L) => U): (Int, Int)

  def foreachFeature[U](f: F => U): (Int, Int) =
    foreach({ (feature, _) => f(feature) })

  final def foreachLabel[U](f: L => U): (Int, Int) =
    foreach({ (_, label) => f(label) })

  def map[F2, L2](f: (F, L) => (F2, L2)): LabeledSet[F2, L2]

  def mapFeature[F2](f: F => F2): LabeledSet[F2, L] =
    map({ (feature, label) => (f(feature), label) })

  final def mapLabel[L2](f: L => L2): LabeledSet[F, L2] =
    map({ (feature, label) => (feature, f(label)) })

}

object LabeledSet {

  def fromIterable[F, L](iterable: Iterable[(F, L)], featureSize: => Int): LabeledSet[F, L] =
    new TraversableOnceLabeledSet[F, L](iterable, featureSize) {
      final def map[F2, L2](f: (F, L) => (F2, L2)): LabeledSet[F2, L2] =
        fromIterable(iterable.map(f.tupled), featureSize)
    }

  def fromTraversable[F, L](traversable: Traversable[(F, L)], featureSize: => Int): LabeledSet[F, L] =
    new TraversableOnceLabeledSet[F, L](traversable, featureSize) {
      final def map[F2, L2](f: (F, L) => (F2, L2)): LabeledSet[F2, L2] =
        fromTraversable(traversable.map(f.tupled), featureSize)
    }

  private abstract class TraversableOnceLabeledSet[F, L](
    traversableOnce: TraversableOnce[(F, L)],
    featureSize: Int
  ) extends LabeledSet[F, L] {

    final def foreach[U](f: (F, L) => U): (Int, Int) = {
      var size = 0
      for ((feature, label) <- traversableOnce) {
        f(feature, label)
        size += 1
      }
      (size, featureSize)
    }

  }

}
