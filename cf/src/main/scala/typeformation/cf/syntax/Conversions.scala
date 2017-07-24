package typeformation.cf.syntax

import shapeless.Witness
import typeformation.cf._
import typeformation.cf.CfExp._

trait Conversions {
  implicit def lit2OptExp[T: IsLit](value: T): Option[CfExp[T]] =
    Some(lit(value))

  implicit def lit2Exp[T: IsLit](value: T): CfExp[T] = lit(value)

  implicit def lit2Token[T: IsLit](value: T): FnSubToken.Literal[T] =
    FnSubToken.Literal(value)

  implicit def logicalId2Token[T <: HasLogicalId](value: T): FnSubToken.LogicalId[T] =
    FnSubToken.LogicalId(value)

  implicit class FnSubResourceSyntax[R <: Resource](resource: R) {
    def attr(w: Witness.Lt[String])(implicit ev: ResourceAtt[R, w.T]): FnSubToken.ResourceAttribute[R] =
      FnSubToken.ResourceAttribute(resource, w.value)
  }
}
