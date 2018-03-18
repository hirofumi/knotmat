package object knotmat {

  type ArrayPair[A, B] = (Array[A], Array[B])
  type FeatureSet[+F] = LabeledSet[F, Unit]

}
