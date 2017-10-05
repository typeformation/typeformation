package typeformation.cf.syntax

import io.circe.Json
import org.scalatest.{FreeSpec, Matchers}
import shapeless.Witness.Lt
import shapeless.test.illTyped
import typeformation.cf.CfExp.{FnJoin, FnSub}
import typeformation.cf._

class InterpolatorsTest extends FreeSpec with Matchers {

  trait TestResource extends Resource {
    override def fqn: String = "TestResource"
    override def logicalId: String = "resource"
    override def DeletionPolicy: Option[ResourceAttributes.DeletionPolicy] =
      None
    override def UpdatePolicy: Option[ResourceAttributes.UpdatePolicy] = None
    override def Metadata: Option[Json] = None
    override def CreationPolicy: Option[ResourceAttributes.CreationPolicy] =
      None
    override def DependsOn: Option[Resource] = None
    override def jsonEncode: Json = ???
  }

  val resource = new TestResource {}

  "fnJoin interpolator" - {
    import Encoding._
    import io.circe.syntax._

    "joins literals" in {
      fnJoin"-${lit(1)}-${lit(2)}-${lit(3)}" should ===(
        FnJoin("",
               List(Json.fromString("-"),
                    Json.fromInt(1),
                    Json.fromString("-"),
                    Json.fromInt(2),
                    Json.fromString("-"),
                    Json.fromInt(3))))
    }
    "joins string expressions" in {
      implicit val dbNameAttr = ResourceAtt.mk[TestResource]("dbName")

      val getDbName = fnGetAtt(resource, "dbName")
      val DBUsername = Parameter.Str("dbUsername").ref
      val DBPassword = Parameter.Str("dbPassword").ref

      fnJoin"""CREATE DATABASE $getDbName;
CREATE USER '$DBUsername'@'localhost' IDENTIFIED BY '$DBPassword';""" should ===(
        FnJoin(
          "",
          List(
            "CREATE DATABASE ".asJson,
            Json.obj(
              "Fn::GetAtt" -> Json.arr("resource".asJson, "dbName".asJson)),
            ";\nCREATE USER '".asJson,
            DBUsername.asJson,
            "'@'localhost' IDENTIFIED BY '".asJson,
            DBPassword.asJson,
            "';".asJson
          )
        )
      )
    }
  }
  "fnSub interpolator" - {
    val param = Parameter.Str("param")
    implicit val resAttr =
      ResourceAtt.mk[TestResource]("testAttr")

    "rejects non-token arguments" in {
      illTyped { """fnSub"$param and $resource and ${'notAToken}"""" }
    }

    "accepts parameters, resources and pseudoparameters as arguments" in {
      fnSub"$param, $resource and ${PseudoParameter.Region}" should ===(
        FnSub("${param}, ${resource} and ${AWS::Region}", None))
    }

    "accepts parameters and resource attributes as arguments" in {
      fnSub"$param and ${resource.attr("testAttr")}" should ===(
        FnSub("${param} and ${resource.testAttr}", None))
    }

    "interpolates literals without quoting them" in {
      fnSub"$param and ${"literal-string"} and ${1}" should ===(
        FnSub("${param} and literal-string and 1", None))
    }

    "rejects invalid resource attributes" in {
      illTyped { """fnSub"$param and ${resource.attr("invalidAttr")}"""" }
    }
  }
}
