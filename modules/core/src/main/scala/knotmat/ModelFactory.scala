package knotmat

trait ModelFactory[-S, +M] extends (S => M) { self =>

  def apply(source: S): M

  override def andThen[M1](f: M => M1): ModelFactory[S, M1] =
    ModelFactory.fromFunction(this andThen f)

  override def compose[S0](f: S0 => S): ModelFactory[S0, M] =
    ModelFactory.fromFunction(this compose f)

}

object ModelFactory {

  object Dense {
    type MultiLabel[A, +M]  = ModelFactory[DataSet.Dense.MultiLabel[A], M]
    type NoLabel[A, +M]     = ModelFactory[DataSet.Dense.NoLabel[A], M]
    type SingleLabel[A, +M] = ModelFactory[DataSet.Dense.SingleLabel[A], M]
  }

  def apply[M]: ModelFactory[M, M] =
    fromFunction(identity)

  def fromFunction[S, M](f: S => M): ModelFactory[S, M] =
    new ModelFactory[S, M] {
      def apply(source: S): M = f(source)
    }

}
