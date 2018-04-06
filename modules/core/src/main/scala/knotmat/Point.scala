package knotmat

sealed trait Point[@specialized(Double, Float, Int, Long) A] {
  def feature: Vec[A]
  def label: Vec[A]
}

object Point {

  sealed trait MultiLabel[@specialized(Double, Float, Int, Long) A] extends Point[A] {
    def label: Vec.Dense[A]
  }

  sealed abstract class NoLabel[@specialized(Double, Float, Int, Long) A: Vec.Empty] extends Point[A] {
    final def label: Vec[A] = Vec.Empty[A]
  }

  sealed trait SingleLabel[@specialized(Double, Float, Int, Long) A] extends Point[A] {
    def label: Vec.Single[A]
  }

  trait Dense[@specialized(Double, Float, Int, Long) A] extends Point[A] {
    def feature: Vec.Dense[A]
  }

  object Dense {

    final case class MultiLabel[@specialized(Double, Float, Int, Long) A](
      feature: Vec.Dense[A],
      label: Vec.Dense[A]
    ) extends Dense[A] with Point.MultiLabel[A]

    final case class NoLabel[@specialized(Double, Float, Int, Long) A](
      feature: Vec.Dense[A]
    ) extends Dense[A] with Point.NoLabel[A]

    final case class SingleLabel[@specialized(Double, Float, Int, Long) A](
      feature: Vec.Dense[A],
      label: Vec.Single[A]
    ) extends Dense[A] with Point.SingleLabel[A]

  }

}
