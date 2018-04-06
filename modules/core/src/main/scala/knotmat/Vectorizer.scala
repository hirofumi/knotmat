package knotmat

abstract class Vectorizer[-A, B] {

  def vectroize(a: A): Vec[B]

}

object Vectorizer {

  def concat[A, B](vs: DenseLike[A, B]*): Vectorizer[A, B] =
    new Vectorizer[A, B] {
      override def vectroize(a: A): Vec[B] =
        Vec.concat(vs.map(_.vectroize(a)): _*)
    }

  def concatDense[A, B](vs: DenseLike[A, B]*): Dense[A, B] =
    new Dense[A, B] {
      override def vectroize(a: A): Vec.Dense[B] =
        Vec.concatDense[B](vs.map(_.vectroize(a)): _*)
    }

  abstract class DenseLike[-A, B] extends Vectorizer[A, B] {

    def vectroize(a: A): Vec.DenseLike[B]

  }

  abstract class Dense[-A, B] extends DenseLike[A, B] {

    def vectroize(a: A): Vec.Dense[B]

  }

  abstract class Single[-A, B] extends Vectorizer[A, B] {

    def vectroize(a: A): Vec.Single[B]

  }

}
