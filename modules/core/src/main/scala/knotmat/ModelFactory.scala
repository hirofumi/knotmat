package knotmat

trait ModelFactory[-S, +M] extends (S => M) { self =>

  def apply(source: S): M

  override def andThen[M1](f: M => M1): ModelFactory[S, M1] =
    ModelFactory.fromFunction(this andThen f)

  override def compose[S0](f: S0 => S): ModelFactory[S0, M] =
    ModelFactory.fromFunction(this compose f)

  def composeFeatureSet[F](f: FeatureSet[F] => S): ModelFactory.ForFeatureSet[F, M] =
    compose(f)

  def composeLabeledSet[F, L](f: LabeledSet[F, L] => S): ModelFactory.ForLabeledSet[F, L, M] =
    compose(f)

}

object ModelFactory {

  type ForFeatureSet[F, +M]    = ForLabeledSet[F, Unit, M]
  type ForLabeledSet[F, L, +M] = ModelFactory[LabeledSet[F, L], M]

  def apply[M]: ModelFactory[M, M] =
    fromFunction(identity)

  def fromFunction[S, M](f: S => M): ModelFactory[S, M] =
    new ModelFactory[S, M] {
      def apply(source: S): M = f(source)
    }

  final implicit class ForLabeledSetSyntax[F, L, M](
    private val self: ForLabeledSet[F, L, M]
  ) extends AnyVal {

    def withCodec[F0, L0](
      featureCodec: FeatureCodec[F0, F],
      labelCodec: LabelCodec[L0, L]
    ): ModelCodec[F0, L0, F, L, M] =
      new ModelCodec[F0, L0, F, L, M] {

        override def decodeFeature(feature: F): F0 =
          featureCodec.decode(feature)

        override def decodeLabel(label: L): L0 =
          labelCodec.decode(label)

        override def apply(source: TraversableOnce[(F0, L0)]): M =
          self.apply(
            new LabeledSet[F, L] {
              override def foreach[U](f: (F, L) => U): (Int, Int) = {
                var i = 0
                for ((feature, label) <- source) {
                  f(featureCodec.encode(feature), labelCodec.encode(label))
                  i += 1
                }
                (i, featureCodec.dim)
              }
            }
          )

      }

    def withFeatureCodec[F0](featureCodec: FeatureCodec[F0, F]): ModelCodec[F0, L, F, L, M] =
      withCodec(featureCodec, LabelCodec[L])

  }

}
