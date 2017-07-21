package typeformation.cf.syntax

import io.circe.Json
import org.scalatest.{FreeSpec, Matchers}
import shapeless.test.illTyped
import typeformation.cf.CfExp.FnSub
import typeformation.cf._

class InterpolatorsTest extends FreeSpec with Matchers {
  "fnSub interpolator" - {
    val param = Parameter.Str("param")

    trait TestResource extends Resource {
      override def fqn: String = "TestResource"
      override def logicalId: String = "resource"
      override def DeletionPolicy: Option[ResourceAttributes.DeletionPolicy] = None
      override def UpdatePolicy: Option[ResourceAttributes.UpdatePolicy] = None
      override def Metadata: Option[Json] = None
      override def CreationPolicy: Option[ResourceAttributes.CreationPolicy] = None
      override def DependsOn: Option[Resource] = None
      override def jsonEncode: Json = ???
    }

    val resource = new TestResource {}

    implicit val resAttr = HasGetAtt.mk[TestResource]("testAttr")

    "rejects non-token arguments" in {
      illTyped { """fnSub"$param and $resource and ${'notAToken}"""" }
    }

    "accepts parameters, resources and pseudoparameters as arguments" in {
      fnSub"$param, $resource and ${PseudoParameter.Region}" should ===(FnSub("${param}, ${resource} and ${AWS::Region}", None))
    }

    "accepts parameters and resource attributes as arguments" in {
      fnSub"$param and ${resource.attr("testAttr")}" should ===(FnSub("${param} and ${resource.testAttr}", None))
    }

    "rejects invalid resource attributes" in {
      illTyped { """fnSub"$param and ${resource.attr("invalidAttr")}"""" }
    }
  }
}
