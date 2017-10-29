package object typeformation {
  type Namespace = String

  /**
    * PropertyType usually defines custom types, but sometimes merely aliases primitive types.
    * This is seen for example on AWS::SSM::PatchBaseLine.PatchGroup
    * In this case, while a custom PatchGroup property type is define, the documentation specify to use
    * it as if it was a string
    */
  sealed trait PropertyType {
    def namespace: Namespace
    def name: String
  }
  case class AliasPropertyType(namespace: Namespace, name: String, primitiveType: PrimitiveType) extends PropertyType
  case class CustomPropertyType(namespace: Namespace, name: String, properties: List[Property]) extends PropertyType

  sealed trait AwsType
  sealed trait PrimitiveType extends AwsType

  case object TagType extends AwsType
  case class PropertyTypeRef(namespace: String, name: String) extends AwsType
  case class ListType(awsType: AwsType) extends AwsType
  case class MapType(awsType: AwsType) extends AwsType

  object PrimitiveType {
    case object String extends PrimitiveType
    case object Long extends PrimitiveType
    case object Integer extends PrimitiveType
    case object Double extends PrimitiveType
    case object Boolean extends PrimitiveType
    case object Timestamp extends PrimitiveType
    case object Json extends PrimitiveType
    val all = Set(String, Long, Integer, Double, Boolean, Timestamp, Json)
    def fromString(str: String): Option[PrimitiveType] = all.find(_.toString == str)
  }

  case class Property(
    name: String,
    awsType: AwsType,
    required: Boolean
  )

  case class ResourceType(fqn: String, properties: List[Property])
}
