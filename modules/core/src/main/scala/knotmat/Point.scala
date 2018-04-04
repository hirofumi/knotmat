package knotmat

sealed trait Point[@specialized(Double, Float, Int, Long) A] {

  def feature: PreVector[A]

  def label: PreVector[A]

}

object Point {

  sealed trait MultiLabel[@specialized(Double, Float, Int, Long) A] extends Point[A] {
    def label: PreVector.Dense[A]
  }

  sealed trait NoLabel[@specialized(Double, Float, Int, Long) A] extends Point[A] {
    final def label: PreVector[A] = PreVector.Empty
  }

  sealed trait SingleLabel[@specialized(Double, Float, Int, Long) A] extends Point[A] {
    def label: PreVector.Single[A]
  }

  trait Dense[@specialized(Double, Float, Int, Long) A] extends Point[A] {
    def feature: PreVector.Dense[A]
  }

  object Dense {

    final case class MultiLabel[@specialized(Double, Float, Int, Long) A](
      feature: PreVector.Dense[A],
      label: PreVector.Dense[A]
    ) extends Dense[A] with Point.MultiLabel[A]

    final case class NoLabel[@specialized(Double, Float, Int, Long) A](
      feature: PreVector.Dense[A]
    ) extends Dense[A] with Point.NoLabel[A]

    final case class SingleLabel[@specialized(Double, Float, Int, Long) A](
      feature: PreVector.Dense[A],
      label: PreVector.Single[A]
    ) extends Dense[A] with Point.SingleLabel[A]

  }

}
