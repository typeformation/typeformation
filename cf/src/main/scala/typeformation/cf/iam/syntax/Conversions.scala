package typeformation.cf.iam.syntax

import typeformation.cf.iam.Invertible
import Invertible._

trait Conversions {

  implicit class InvertibleSyntax[A: IsInvertible](value: A) {
    def neg: Invertible[A] = Neg(value)
    def pos: Invertible[A] = Pos(value)
  }

  implicit def implicitInvertiblePos[A: IsInvertible](a: A): Invertible[A] =
    a.pos
}
