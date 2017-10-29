package typeformation.iam

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
  object IsInvertible {
    implicit val actionIsInvertible: IsInvertible[Action] =
      new IsInvertible[Action] {}
    implicit val principalIsInvertible: IsInvertible[Principal] =
      new IsInvertible[Principal] {}
    implicit val arnListInvertible: IsInvertible[List[Arn]] =
      new IsInvertible[List[Arn]] {}
  }
}
