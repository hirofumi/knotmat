package knotmat

final class LabeledSetCodec[M, F1, F2, L1, L2] private[knotmat](
  convert: LabeledSet[F2, L2] => M,
  featureCodec: DataCodec[F1, F2],
  labelCodec: DataCodec[L1, L2]
) {

  def addFeatureCodec[F0](c: DataCodec[F0, F1]): LabeledSetCodec[M, F0, F2, L1, L2] =
    new LabeledSetCodec(convert, c + featureCodec, labelCodec)

  def addLabelCodec[L0](c: DataCodec[L0, L1]): LabeledSetCodec[M, F1, F2, L0, L2] =
    new LabeledSetCodec(convert, featureCodec, c + labelCodec)

  def decodeFeature(feature: F2): F1 =
    featureCodec.decode(feature)

  def decodeLabel(label: L2): L1 =
    labelCodec.decode(label)

  def encode(labeled: LabeledSet[F1, L1]): M =
    convert(labeled.map({ case (f, l) => featureCodec.encode(f) -> labelCodec.encode(l) }))

  def map[M1](f: M => M1): LabeledSetCodec[M1, F1, F2, L1, L2] =
    new LabeledSetCodec(convert.andThen(f), featureCodec, labelCodec)

}

object LabeledSetCodec {

  type Simple[M, A, B] = LabeledSetCodec[M, A, A, B, B]

  def apply[A, B]: LabeledSetCodec[LabeledSet[A, B], A, A, B, B] =
    new LabeledSetCodec(identity, DataCodec[A], DataCodec[B])

}
