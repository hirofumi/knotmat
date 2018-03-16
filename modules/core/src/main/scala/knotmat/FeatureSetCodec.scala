package knotmat

object FeatureSetCodec {

  def apply[F]: FeatureSetCodec[FeatureSet[F], F, F] =
    new FeatureSetCodec(identity, DataCodec[F], DataCodec[Unit])

}
