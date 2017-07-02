package typeformation.cf

case class Condition(logicalId: String,
                     value: CfExp[Boolean]) extends HasLogicalId
