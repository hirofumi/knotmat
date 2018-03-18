package knotmat

trait LabeledSet[F, L] {

  def foreach[U](f: (F, L) => U): (Int, Int)

}
