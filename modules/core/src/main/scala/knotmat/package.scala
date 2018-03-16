package object knotmat {

  type ArrayPair[A, B] = (Array[A], Array[B])
  type FeatureSet[+F] = LabeledSet[F, Unit]
  type FeatureSetCodec[M, F1, F2] = LabeledSetCodec[M, F1, F2, Unit, Unit]

}
