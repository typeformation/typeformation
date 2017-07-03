package typeformation.cf.syntax

import typeformation.cf.CfExp
import typeformation.cf.CfExp._

trait Conversions {
  implicit def litToOptExp[T: IsLit](value: T): Option[CfExp[T]] =
    Some(lit(value))

  implicit def litToExp[T: IsLit](value: T): CfExp[T] = lit(value)
}
