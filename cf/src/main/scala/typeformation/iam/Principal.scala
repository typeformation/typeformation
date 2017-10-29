package typeformation.iam

import typeformation.cf.Arn
import typeformation.cf.CfExp

sealed trait Principal
object Principal {
  case class Aws(arns: Arn*) extends Principal
  case class Federated(value: CfExp[String]) extends Principal
  case class CanonicalUser(value: CfExp[String]) extends Principal
  case class Service(values: CfExp[String]*) extends Principal
}
