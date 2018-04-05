package knotmat

abstract class Vectorizer[A, B] {

  def vectroize(a: A): B

}

object Vectorizer {

  type Dense[A, B]  = Vectorizer[A, PreVector.Dense[B]]
  type Single[A, B] = Vectorizer[A, PreVector.Single[B]]

  implicit class DenseSyntax[A, B](private val self: Dense[A, B]) extends AnyVal {

    def ++(that: Single[A, B]): Dense[A, B] =
      new Dense[A, B] {
        override def vectroize(a: A): PreVector.Dense[B] =
          self.vectroize(a) ++ that.vectroize(a)
      }

    def ++(that: Dense[A, B]): Dense[A, B] =
      new Dense[A, B] {
        override def vectroize(a: A): PreVector.Dense[B] =
          self.vectroize(a) ++ that.vectroize(a)
      }

  }

}
