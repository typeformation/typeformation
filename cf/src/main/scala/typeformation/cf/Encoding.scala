package typeformation.cf

import java.net.{InetAddress, URL}
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import typeformation.cf.CfExp._
import typeformation.cf.Parameter.{AwsParamType, CommaDelimited}
import AwsParamType._
import io.circe.syntax._
import typeformation.cf.init._
import io.circe.{Encoder, Json, JsonObject}
import java.time.Duration

import enum.Enum
import ResourceAttributes._
import io.circe.generic.semiauto.deriveEncoder

import scala.util.Try

object Encoding {

  implicit final val encodeDuration: Encoder[Duration] =
    Encoder.instance(duration => Json.fromString(duration.toString))

  implicit val encodeZonedDateTime: Encoder[ZonedDateTime] =
    implicitly[Encoder[String]]
      .contramap[ZonedDateTime](_.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))

  implicit val encodeTagExp: Encoder[Tag] =
    Encoder.instance[Tag] { t =>
      Json.obj(
        "Key" -> Json.fromString(t.key),
        "Value" -> Json.fromString(t.value)
      )
    }

  implicit val encodeInetIp =
    Encoder.instance[InetAddress](a => Json.fromString(a.toString))
  implicit val encodeArn = Encoder.instance[Arn](a => Json.fromString(a.value))

  implicit def encodeLit[T: IsLit: Encoder]: Encoder[Lit[T]] =
    implicitly[Encoder[T]].contramap[Lit[T]](_.value)

  implicit def encodeCfExp[T: IsLit: Encoder]: Encoder[CfExp[T]] =
    Encoder.instance[CfExp[T]] {
      case l: Lit[T] =>
        l.asJson
      case FnBase64(exp) =>
        Json.obj("Fn::Base64" -> exp.asJson)
      case FnAnd(cond1, cond2) =>
        Json.obj("Fn::And" -> List(cond1, cond2).asJson)
      case eq: FnEquals[T @unchecked] =>
        Json.obj(
          "Fn::Equals" -> Json.fromValues(
            List(eq.left.asJson, eq.right.asJson)))
      case FnIf(cond, ifTrue, ifFalse) =>
        Json.obj(
          "Fn::If" -> Json.fromValues(
            List(cond.asJson, ifTrue.asJson, ifFalse.asJson)))
      case FnNot(cond) =>
        Json.obj("Fn::Not" -> List(cond).asJson) // weird but correct according to the doc
      case FnJoin(delimiter, chunks) =>
        Json.obj("Fn::Join" -> Json.arr(delimiter.asJson, chunks.asJson))
      case FnOr(conds) =>
        Json.obj("Fn::Or" -> conds.asJson)
      case FnGetAZs(region) =>
        Json.obj(
          "Fn::GetAZs" ->
            Json.fromString(region.getOrElse("")))
      case FnSplit(separator, string) =>
        Json.obj(
          "Fn::Split" -> Json.arr(
            Json.fromString(separator),
            string.asJson
          ))
      case FnFindInMap(m, key1, key2) =>
        Json.obj(
          "Fn::FindInMap" -> Json.arr(
            m.logicalId.asJson,
            key1.asJson,
            key2.asJson
          ))
      case FnSelect(index, values) =>
        Json.obj(
          "Fn::Select" -> Json.arr(
            Json.fromInt(index),
            values.asJson
          ))
      case FnSub(str, mappings) =>
        Json.obj(
          "Fn::Sub" ->
            mappings.fold(Json.fromString(str))(
              m =>
                Json.arr(
                  Json.fromString(str),
                  m.asJson
              )))
      case FnGetAtt(logicalId, attr) =>
        Json.obj(
          "Fn::GetAtt" -> Json.arr(Json.fromString(logicalId),
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

  implicit def encodeEnum[T: Enum]: Encoder[T] = Encoder.instance[T] { t =>
    Json.fromString(implicitly[Enum[T]].encode(t))
  }

  implicit def encodeEnumMap[K, V](
      implicit
      enum: Enum[K],
      mapEnc: Encoder[Map[String, V]]
  ): Encoder[Map[K, V]] =
    mapEnc.contramap[Map[K, V]](_.map { case (k, v) => enum.encode(k) -> v })

  implicit val encodeParamDataType: Encoder[Parameter.DataType] = {
    import Parameter.DataType._
    val awsEnum = implicitly[Enum[Parameter.AwsParamType]]

    Encoder.instance[Parameter.DataType] { dt =>
      val s = dt match {
        case String             => "String"
        case Number             => "Number"
        case `List<Number>`     => "List<Number>"
        case CommaDelimitedList => "CommaDelimitedList"
        case t: AwsTypeList     => s"List<${awsEnum.encode(t.tpe)}>"
        case t: AwsType         => awsEnum.encode(t.tpe)
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

  implicit val encodeAutoScalingReplacingUpdate
    : Encoder[AutoScalingReplacingUpdate] =
    deriveEncoder[AutoScalingReplacingUpdate]

  implicit val encodeAutoScalingRollingUpdate
    : Encoder[AutoScalingRollingUpdate] =
    deriveEncoder[AutoScalingRollingUpdate]

  implicit val encodeAutoScalingScheduledAction
    : Encoder[AutoScalingScheduledAction] =
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
    Json.obj(
      o.logicalId -> Json
        .obj(
          "Description" -> o.Description.asJson,
          "Value" -> o.Value.asJson,
          "Condition" -> o.Condition.map(_.logicalId).asJson
        )
        .deepMerge(o.Export.fold(Json.obj()) { e =>
          Json.obj("Export" -> Json.obj("Name" -> e.asJson))
        }))
  }

  implicit val encodeMapping: Encoder[Template.Mapping] = Encoder.instance {
    o =>
      Json.obj(o.logicalId -> o.value.asJson)
  }

  implicit val encodeTemplate: Encoder[Template] = Encoder.instance { t =>
    Json.obj(
      "AWSTemplateFormatVersion" -> Json.fromString("2010-09-09"),
      "Description" -> t.Description.asJson,
      "Metadata" -> t.Metadata.asJson,
      "Parameters" -> foldKeyVals(t.Parameters),
      "Mappings" -> foldKeyVals(t.Mappings),
      "Conditions" -> foldKeyVals(t.Conditions),
      "Resources" -> foldKeyVals(t.Resources.map(_.jsonEncode)),
      "Outputs" -> foldKeyVals(t.Outputs)
    )
  }

  implicit val cfInitCommandEncoder: Encoder[Command] = {
    val enc: Encoder[Command] = deriveEncoder[Command]
    Encoder.instance { c =>
      Json.obj(c.logicalId -> enc(c).withObject(_.remove("logicalId").asJson))
    }
  }

  implicit def cfInitFileEncoder[T <: File]: Encoder[T] = {
    lazy val encS: Encoder[File.FromString] = deriveEncoder[File.FromString]
    lazy val encU: Encoder[File.FromUrl] = deriveEncoder[File.FromUrl]

    Encoder.instance { f =>
      val encoded = f match {
        case fS: File.FromString => encS(fS)
        case fU: File.FromUrl    => encU(fU)
      }
      Json.obj(f.path -> encoded.withObject(_.remove("path").asJson))
    }
  }

  implicit val cfInitGroupEnc: Encoder[Group] = Encoder.instance { g =>
    Json.obj(
      g.logicalId -> Json.obj(
        "gid" -> g.gid.asJson
      ))
  }

  implicit val cfInitPackagesEncoder: Encoder[Package] = Encoder.instance { p =>
    val version = p.version match {
      case Nil      => Json.arr()
      case v :: Nil =>
        //lists of one signle URLs should be encoded as string
        if (Try { new URL(v) }.isSuccess)
          Json.fromString(v)
        else
          p.version.asJson
      case other => p.version.asJson
    }
    Json.obj(p.name -> version)
  }

  implicit val cfInitServiceEncoder: Encoder[Service] = Encoder.instance {
    srv =>
      val enc: Encoder[Service] = deriveEncoder[Service]
      Json.obj(srv.name -> enc(srv).withObject(_.remove("name").asJson))
  }

  implicit val cfInitSource: Encoder[Source] = Encoder.instance { src =>
    Json.obj(src.targetDir -> Json.fromString(src.url))
  }

  implicit val cfInitUser: Encoder[User] = Encoder.instance { u =>
    Json.obj(
      u.username -> Json.obj(
        "groups" -> u.groups.asJson,
        "uid" -> u.uid.asJson,
        "homeDir" -> u.homeDir.asJson
      ))
  }

  implicit def cfnInitServiceListEncoder[S <: Service.Services]: Encoder[S] =
    Encoder.instance { srvs =>
      val obj = emptyToNull(foldKeyVals(srvs.toList))

      srvs match {
        case Service.Sysvinit(_) =>
          Json.obj("sysvinit" -> obj)
        case Service.Windows(_) =>
          Json.obj("windows" -> obj)
      }
    }

  implicit val cfnInitConfigEncoder: Encoder[Init.Config] = Encoder.instance {
    c =>
      val pkgsByFormat: Map[String, Json] =
        c.packages
          .groupBy(p => implicitly[Enum[PackageFormat]].encode(p.format))
          .mapValues(foldKeyVals(_))

      def f = emptyToNull _

      Json.obj(
        c.logicalId -> Json.obj(
          "commands" -> f(foldKeyVals(c.commands)),
          "files" -> f(foldKeyVals(c.files)),
          "groups" -> f(foldKeyVals(c.groups)),
          "packages" -> pkgsByFormat.asJson,
          "services" -> c.services.asJson,
          "sources" -> f(foldKeyVals(c.sources)),
          "users" -> f(foldKeyVals(c.users))
        )
      )
  }

  implicit val cfnInitEncoder: Encoder[Init] = Encoder.instance { i =>
    Json.obj(
      "AWS::CloudFormation::Init" -> Json
        .obj(
          "configSets" -> i.configSets.asJson
        )
        .deepMerge(foldKeyVals(i.configs.toList))
    )
  }

  implicit val iamActionEncoder: Encoder[iam.Action] =
    implicitly[Encoder[Set[String]]].contramap(_.value)

  implicit val encodeIamPrincipal: Encoder[iam.Principal] = {
    import iam.Principal._
    Encoder.instance {
      case Aws(arns)            => Json.obj("AWS" -> unwrapSingleton(arns))
      case Service(values)      => Json.obj("Service" -> values.asJson)
      case CanonicalUser(value) => Json.obj("CanonicalUser" -> value.asJson)
      case Federated(value)     => Json.obj("Federated" -> value.asJson)
    }
  }

  implicit val encodeIamCondition: Encoder[iam.Condition] = Encoder.instance {
    c =>
      val suffix = if (c.hasIfExists) "IfExists" else ""
      val labelWithModifiers =
        c.quantifier.fold[String](c.label)(q => s"${q.id}:${c.label}") ++ suffix

      Json.obj(
        labelWithModifiers -> Json.obj(
          c.key.value -> unwrapSingleton(c.expressionEncoder(c.expected)))
      )
  }

  implicit val encodeStatement: Encoder[iam.Statement] = Encoder.instance { s =>
    def invertibleKV[A: Encoder](key: String,
                                 inv: iam.Invertible[A]): (String, Json) =
      (if (inv.isPositive) key else s"Not$key") -> unwrapSingleton(
        inv.value.asJson)

    Json.obj(
      Seq(
        "Sid" -> s.Sid.asJson,
        "Effect" -> Json.fromString(iam.Effect.effectEnum.encode(s.Effect)),
        invertibleKV("Resource", s.Resource),
        invertibleKV("Action", s.Action),
        "Condition" -> foldKeyVals(s.Condition)
      ) ++ s.Principal
        .map(p => Seq(invertibleKV("Principal", p)))
        .getOrElse(Nil): _*)
  }

  implicit val encodePolicy: Encoder[iam.Policy] = Encoder.instance { p =>
    Json.obj("Id" -> p.Id.asJson,
             "Version" -> p.Version.asJson,
             "Statement" -> p.Statement.asJson)
  }

  private[cf] def unwrapSingleton(j: Json): Json =
    j.withArray(arr => if (arr.size == 1) arr.head else arr.asJson)

  private[cf] def unwrapSingleton[A: Encoder](as: Traversable[A]): Json =
    if (as.size == 1) as.head.asJson else as.asJson

  private[cf] def emptyToNull(o: Json): Json = {
    val onArray: Vector[Json] => Json = arr =>
      if (arr.isEmpty) Json.Null else arr.map(emptyToNull).asJson

    val onObject: JsonObject => Json = obj =>
      if (obj.isEmpty)
        Json.Null
      else
        Json.obj(obj.toList.map {
          case (k, v) =>
            k -> emptyToNull(v)
        }: _*)

    o.arrayOrObject(o, onArray, onObject)
  }

  private[cf] def dropNull(o: Json): Json = {
    val onArray: Vector[Json] => Json = arr =>
      arr.foldLeft(Json.arr()) { (arr, el) =>
        if (el.isNull)
          arr
        else
          arr.withArray(x => (x :+ dropNull(el)).asJson)
    }
    val onObject: JsonObject => Json = obj =>
      if (obj.isEmpty)
        o
      else
        obj.toList
          .foldLeft(JsonObject.empty) {
            case (o, (k, v)) =>
              if (v.isNull) o else o.add(k, dropNull(v))
          }
          .asJson

    o.arrayOrObject(o, onArray, onObject)
  }

  private def foldKeyVals[A: Encoder](objects: List[A]): Json =
    if (objects.isEmpty)
      Json.Null
    else
      objects.foldRight(Json.obj()) {
        case (item, o) =>
          o.deepMerge(item.asJson)
      }
}
