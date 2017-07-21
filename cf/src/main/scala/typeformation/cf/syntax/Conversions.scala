package typeformation.cf.syntax

import shapeless.Witness
import typeformation.cf._
import typeformation.cf.CfExp._

trait Conversions {
  implicit def lit2OptExp[T: IsLit](value: T): Option[CfExp[T]] =
    Some(lit(value))

  implicit def lit2Exp[T: IsLit](value: T): CfExp[T] = lit(value)

  implicit def logicalId2Token[T <: HasLogicalId](t: T): FnSubToken.LogicalId[T] =
    FnSubToken.LogicalId(t)

  implicit class FnSubResourceSyntax[R <: Resource](resource: R) {
    def attr(w: Witness.Lt[String])(implicit ev: ResourceAtt[R, w.T]): FnSubToken.ResourceAttribute[R] =
      FnSubToken.ResourceAttribute(resource, w.value)
  }
}
