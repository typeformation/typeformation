package typeformation.cf.iam

import typeformation.cf.Arn

sealed trait Invertible[T] {
  def value: T
  def isPositive: Boolean
}

object Invertible {
  case class Pos[T](value: T) extends Invertible[T] {
    override def isPositive = true
  }
  case class Neg[T](value: T) extends Invertible[T] {
    override def isPositive = false
  }

  trait IsInvertible[A]
  implicit val actionIsInvertiblew: IsInvertible[Action] =
    new IsInvertible[Action] {}
  implicit val principalIsInvertiblew: IsInvertible[Principal] =
    new IsInvertible[Principal] {}
  implicit val arnListInvertiblew: IsInvertible[List[Arn]] =
    new IsInvertible[List[Arn]] {}
}
