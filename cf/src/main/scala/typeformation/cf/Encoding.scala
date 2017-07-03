package typeformation.cf

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import typeformation.cf.CfExp._
import typeformation.cf.Parameter.{AwsParamType, CommaDelimited}
import AwsParamType._
import io.circe.syntax._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Encoder, Json}
import java.time.Duration
import enum.Enum

import ResourceAttributes._

object Encoding {

  implicit final val encodeDuration: Encoder[Duration] =
    Encoder.instance(duration => Json.fromString(duration.toString))

  implicit val encodeZonedDateTime: Encoder[ZonedDateTime] =
    implicitly[Encoder[String]].contramap[ZonedDateTime](_.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))

  implicit val encodeTagExp: Encoder[Tag] =
    Encoder.instance[Tag] { t =>
      Json.obj(
        "Key" -> Json.fromString(t.key),
        "Value" -> Json.fromString(t.value)
      )
    }

  implicit def encodeLit[T : IsLit : Encoder]: Encoder[Lit[T]] =
    implicitly[Encoder[T]].contramap[Lit[T]](_.value)

  implicit def encodeCfExp[T: IsLit : Encoder]: Encoder[CfExp[T]] = Encoder.instance[CfExp[T]] {
    case l: Lit[T] =>
      l.asJson
    case FnBase64(exp) =>
      Json.obj("Fn::Base64" -> exp.asJson)
    case FnAnd(cond1, cond2) =>
      Json.obj("Fn::And" -> List(cond1, cond2).asJson)
    case eq: FnEquals[T @unchecked] =>
      Json.obj("Fn::Equals" -> Json.fromValues(List(eq.left.asJson, eq.right.asJson)))
    case FnIf(cond, ifTrue, ifFalse) =>
      Json.obj("Fn::If" -> Json.fromValues(List(cond.asJson, ifTrue.asJson, ifFalse.asJson)))
    case FnNot(cond) =>
      Json.obj("Fn::Not" -> List(cond).asJson) // weird but correct according to the doc
    case FnJoin(delimiter, chunks) =>
      Json.obj("Fn::Join" -> Json.arr(delimiter.asJson, chunks.asJson))
    case FnOr(conds) =>
      Json.obj("Fn::Or" -> conds.asJson)
    case FnGetAZs(region) =>
      Json.obj("Fn::GetAZs" ->
        Json.fromString(region.getOrElse(""))
      )
    case FnSplit(separator, string) =>
      Json.obj("Fn::Split" -> Json.arr(
        Json.fromString(separator),
        string.asJson
    ))
    case FnFindInMap(m, key1, key2) =>
      Json.obj("Fn::FindInMap" -> Json.arr(
        m.logicalId.asJson,
        key1.asJson,
        key2.asJson
      ))
    case FnSelect(index, values) =>
      Json.obj("Fn::Select" -> Json.arr(
        Json.fromInt(index),
        values.asJson
      ))
    case FnSub(str, mappings) =>
      Json.obj("Fn::Sub" ->
        mappings.fold(Json.fromString(str))(m =>
          Json.arr(
            Json.fromString(str),
            m.asJson
          )
        )
      )
    case FnGetAtt(logicalId, attr) =>
      Json.obj("Fn::GetAtt" -> Json.arr(
        Json.fromString(logicalId),
        Json.fromString(attr)))
    case ResourceRef(resource) =>
      Json.obj("Ref" -> Json.fromString(resource.logicalId))
    case ParameterRef(resource) =>
      Json.obj("Ref" -> Json.fromString(resource.logicalId))
    case PseudoParameterRef(param) =>
      Json.obj("Ref" -> Json.fromString(param.toString))
    case x =>
      throw new Exception(s"Unexpected expression $x")
  }

  implicit def encodeEnum[T: Enum]
    : Encoder[T] = Encoder.instance[T] { t =>
    Json.fromString(implicitly[Enum[T]].encode(t))
  }


  implicit val encodeParamDataType: Encoder[Parameter.DataType] = {
    import Parameter.DataType._
    val awsEnum = implicitly[Enum[Parameter.AwsParamType]]

    Encoder.instance[Parameter.DataType] { dt =>
      val s = dt match {
        case String => "String"
        case Number => "Number"
        case `List<Number>` => "List<Number>"
        case CommaDelimitedList => "CommaDelimitedList"
        case t: AwsTypeList => s"List<${awsEnum.encode(t.tpe)}>"
        case t: AwsType => awsEnum.encode(t.tpe)
      }
      Json.fromString(s)
    }
  }

  implicit val encodeAutoscalingPolicy: Encoder[AutoscalingCreationPolicy] =
    deriveEncoder[AutoscalingCreationPolicy]

  implicit val encodeResourceSignal: Encoder[ResourceSignal] =
    deriveEncoder[ResourceSignal]

  implicit val encodeCreationPolicy: Encoder[CreationPolicy] =
    deriveEncoder[CreationPolicy]

  implicit val encodeAutoScalingReplacingUpdate: Encoder[AutoScalingReplacingUpdate] =
    deriveEncoder[AutoScalingReplacingUpdate]

  implicit val encodeAutoScalingRollingUpdate: Encoder[AutoScalingRollingUpdate] =
    deriveEncoder[AutoScalingRollingUpdate]

  implicit val encodeAutoScalingScheduledAction: Encoder[AutoScalingScheduledAction] =
    deriveEncoder[AutoScalingScheduledAction]

  implicit val encodeUpdatePolicy: Encoder[UpdatePolicy] =
    deriveEncoder[UpdatePolicy]

  implicit val encodeParam: Encoder[Parameter] =
    Encoder.instance[Parameter] { p =>
      val common = Json.obj(
        "Type" -> p.Type.asJson,
        "Description" -> p.Description.asJson,
        "NoEcho" -> p.NoEcho.asJson
      )

      val specific = p match {
        case sp: Parameter.Str =>
          Json.obj(
            "MaxLength" -> sp.MaxLength.asJson,
            "MinLength" -> sp.MinLength.asJson,
            "AllowedPattern" -> sp.AllowedPattern.asJson,
            "ConstraintDescription" -> sp.ConstraintDescription.asJson,
            "AllowedValues" -> sp.AllowedValues.asJson,
            "Default" -> sp.Default.asJson
          )
        case np: Parameter.Integer =>
          Json.obj(
            "MaxValue" -> np.MaxValue.asJson,
            "MinValue" -> np.MinValue.asJson,
            "Default" -> np.Default.asJson
          )
        case np: Parameter.Double =>
          Json.obj(
            "MaxValue" -> np.MaxValue.asJson,
            "MinValue" -> np.MinValue.asJson,
            "Default" -> np.Default.asJson
          )
        case p: CommaDelimited =>
          Json.obj(
            "AllowedValues" -> p.AllowedValues.map(_.mkString(",")).asJson,
            "Default" -> p.Default.map(_.mkString(",")).asJson
          )
        case _ => Json.obj()
      }

      Json.obj(p.logicalId -> common.deepMerge(specific))
    }

  implicit val encodeCondition: Encoder[Condition] = deriveEncoder[Condition]

  implicit val encodeOutput: Encoder[Output] = Encoder.instance { o =>
    Json.obj(o.logicalId -> Json.obj(
      "Description" -> o.Description.asJson,
      "Value" -> o.Value.asJson,
      "Condition" -> o.Condition.map(_.logicalId).asJson
    ).deepMerge(o.Export.fold(Json.obj()){ e =>
      Json.obj("Export" -> Json.obj("Name" -> e.asJson))
    }))
  }

  implicit val encodeMapping: Encoder[Template.Mapping] = Encoder.instance { o =>
    Json.obj(o.logicalId -> o.value.asJson)
  }

  private def fold[A: Encoder](objects: List[A]): Json =
    if (objects.isEmpty)
      Json.Null
    else
      objects.foldLeft(Json.obj()) { case (o, item) =>
      o.deepMerge(item.asJson)
    }

  implicit val encodeTemplate: Encoder[Template] = Encoder.instance { t =>
    Json.obj(
      "AWSTemplateFormatVersion" -> Json.fromString("2010-09-09"),
      "Description" -> t.Description.asJson,
      "Metadata" -> t.Metadata.asJson,
      "Parameters" -> fold(t.Parameters),
      "Mappings" -> fold(t.Mappings),
      "Conditions" -> fold(t.Conditions),
      "Resources" -> fold(t.Resources.map(_.jsonEncode)),
      "Outputs" -> fold(t.Outputs)
    )
  }
}
