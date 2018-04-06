package knotmat

abstract class Enc[A, @specialized(Double, Float, Int, Long) B] {

  def dim: Int
  def apply(a: A): B

}
