package knotmat

trait ModelCodec[F1, L1, F2, L2, +M] extends ModelFactory[TraversableOnce[(F1, L1)], M] {

  def decodeFeature(feature: F2): F1
  def decodeLabel(label: L2): L1

}

object ModelCodec {

  type Simple[F, L, M] = ModelCodec[F, L, F, L, M]

}
