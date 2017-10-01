package typeformation.cf

import CfExp.ParameterRef
import enum.Enum

sealed trait Parameter extends HasLogicalId with HasRef {
  def logicalId: String
  def Type: Parameter.DataType
  def Description: Option[String]
  def NoEcho: Option[Boolean]
  def ConstraintDescription: Option[String]
  override def ref: CfExp[String] = ParameterRef(this)
}

object Parameter {
  sealed trait AwsParamType

  object AwsParamType {
    case object `AWS::EC2::AvailabilityZone::Name` extends AwsParamType
    case object `AWS::EC2::Instance::Id` extends AwsParamType
    case object `AWS::EC2::Image::Id` extends AwsParamType
    case object `AWS::EC2::KeyPair::KeyName` extends AwsParamType
    case object `AWS::EC2::SecurityGroup::GroupName` extends AwsParamType
    case object `AWS::EC2::SecurityGroup::Id` extends AwsParamType
    case object `AWS::EC2::Subnet::Id` extends AwsParamType
    case object `AWS::EC2::Volume::Id` extends AwsParamType
    case object `AWS::EC2::VPC::Id` extends AwsParamType
    case object `AWS::Route53::HostedZone::Id` extends AwsParamType

    implicit val awsParamEnum: Enum[AwsParamType] = Enum.derived[AwsParamType]
  }

  sealed trait DataType
  object DataType {
    case object String extends DataType
    case object Number extends DataType
    case object `List<Number>` extends DataType
    case object CommaDelimitedList extends DataType
    case class AwsType(tpe: Parameter.AwsParamType) extends DataType
    case class AwsTypeList(tpe: Parameter.AwsParamType) extends DataType
  }

  case class Str(logicalId: String,
                 MaxLength: Option[Int] = None,
                 MinLength: Option[Int] = None,
                 Description: Option[String] = None,
                 NoEcho: Option[Boolean] = None,
                 AllowedValues: Option[Set[String]] = None,
                 AllowedPattern: Option[String] = None,
                 Default: Option[String] = None,
                 ConstraintDescription: Option[String] = None) extends Parameter {
    override def Type: DataType = DataType.String
  }

  case class Double(logicalId: String,
                    MaxValue: Option[scala.Double] = None,
                    MinValue: Option[scala.Double] = None,
                    Description: Option[String] = None,
                    NoEcho: Option[Boolean] = None,
                    Default: Option[scala.Double] = None,
                    ConstraintDescription: Option[String] = None) extends Parameter {
    override val Type = DataType.Number
  }
  case class Integer(logicalId: String,
                    MaxValue: Option[Int] = None,
                    MinValue: Option[Int] = None,
                    Description: Option[String] = None,
                    NoEcho: Option[Boolean] = None,
                    Default: Option[Int] = None,
                    ConstraintDescription: Option[String] = None) extends Parameter {
    override val Type = DataType.Number
  }

  case class CommaDelimited(logicalId: String,
                            AllowedValues: Option[Set[String]] = None,
                            Description: Option[String] = None,
                            NoEcho: Option[Boolean] = None,
                            Default: Option[Set[String]] = None,
                            ConstraintDescription: Option[String] = None) extends Parameter {
    override val Type = DataType.CommaDelimitedList
  }

  case class Aws(logicalId: String,
                 awsType: Parameter.AwsParamType,
                 Description: Option[String] = None,
                 NoEcho: Option[Boolean] = None,
                 Default: Option[String] = None,
                 ConstraintDescription: Option[String] = None) extends Parameter {
    override def Type = DataType.AwsType(awsType)
  }

  case class AwsList(logicalId: String,
                     awsType: Parameter.AwsParamType,
                     Description: Option[String] = None,
                     NoEcho: Option[Boolean] = None,
                     Default: Option[String] = None,
                     ConstraintDescription: Option[String] = None) extends Parameter {
    override def Type = DataType.AwsTypeList(awsType)
  }
}
