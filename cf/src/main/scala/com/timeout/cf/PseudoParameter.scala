package com.timeout.cf

import com.timeout.cf.CfExp.PseudoParameterRef

trait PseudoParameter extends HasRef {
  def toString: String
  override def ref: CfExp[String] = PseudoParameterRef(this)
}

object PseudoParameter {
  case object AccountId extends PseudoParameter{
    override def toString = "AWS::AccountId"
  }

  case object NotificationARNs extends PseudoParameter{
    override def toString = "AWS::NotificationARNs"
  }

  case object NoValue extends PseudoParameter{
    override def toString = "AWS::NoValue"
  }

  case object Region extends PseudoParameter{
    override def toString = "AWS::Region"
  }

  case object StackId extends PseudoParameter{
    override def toString = "AWS::StackId"
  }

  case object StackName extends PseudoParameter{
    override def toString = "AWS::StackName"
  }
}
