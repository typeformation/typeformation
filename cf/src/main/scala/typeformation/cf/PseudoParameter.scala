package typeformation.cf

import CfExp.PseudoParameterRef

trait PseudoParameter extends HasRef with HasLogicalId {
  override def toString = logicalId
  override def ref: CfExp[String] = PseudoParameterRef(this)
}

object PseudoParameter {
  case object AccountId extends PseudoParameter{
    override def logicalId = "AWS::AccountId"
  }

  case object NotificationARNs extends PseudoParameter{
    override def logicalId = "AWS::NotificationARNs"
  }

  case object NoValue extends PseudoParameter{
    override def logicalId = "AWS::NoValue"
  }

  case object Region extends PseudoParameter{
    override def logicalId = "AWS::Region"
  }

  case object StackId extends PseudoParameter{
    override def logicalId = "AWS::StackId"
  }

  case object StackName extends PseudoParameter{
    override def logicalId = "AWS::StackName"
  }
}
