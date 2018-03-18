package knotmat

abstract class LabelCodec[
@specialized(Double, Float, Int, Long) A1,
@specialized(Double, Float, Int, Long) A2
] { self =>

  def encode(a1: A1): A2
  def decode(a2: A2): A1

  def +[A3](that: LabelCodec[A2, A3]): LabelCodec[A1, A3] =
    new LabelCodec {
      final def encode(a1: A1): A3 = that.encode(self.encode(a1))
      final def decode(a3: A3): A1 = self.decode(that.decode(a3))
    }

}

object LabelCodec {

  def apply[@specialized(Double, Float, Int, Long) A]: LabelCodec[A, A] =
    new LabelCodec[A, A] {
      override final def encode(a: A): A = a
      override final def decode(a: A): A = a
    }

}
