package typeformation.cf.cloudinit
import typeformation.cf.{CfExp, HasLogicalId}

case class Command(logicalId: String,
                   command: String,
                   cwd: Option[String] = None,
                   test: Option[String] = None,
                   ignoreErrors: Option[Boolean] = None,
                   env: Map[String, CfExp[String]] = Map.empty,
                   waitAfterCompletion: Option[Int] = None) extends HasLogicalId