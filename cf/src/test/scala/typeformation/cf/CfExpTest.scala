package typeformation.cf

import CfExp._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{FreeSpec, Matchers}
import Encoding._
import io.circe.{Encoder, Json}

object CfExpTest {
  case class TestResource(logicalId: String, foo: CfExp[String]) extends Resource {
    override def fqn: String = "AWS::Test::TestResource"
    override def jsonEncode: Json =
      Json.obj(
        logicalId -> Json.obj(
          "Type" -> Json.fromString(fqn),
          "Properties" -> Json.obj("foo" -> foo.asJson)
        )
      )

    override val DependsOn = None
    override val UpdatePolicy = None
    override val DeletionPolicy = None
    override val CreationPolicy = None
    override val Metadata = None
  }

  implicit val enc = Encoder.instance[TestResource](_.jsonEncode)
}

import CfExpTest._

class CfExpTest extends FreeSpec with Matchers {
  "Literals are handled" in {
    val exp =
      """
        |{
        |  "Logical ID": {
        |    "Type": "AWS::Test::TestResource",
        |    "Properties": {
        |      "foo": "bar"
        |    }
        |  }
        |}
      """.stripMargin
    val expJson = parse(exp)

    val actual = TestResource(
      logicalId = "Logical ID", foo = Lit("bar")
    ).asJson

    Right(actual) should ===(expJson)
  }

  "Fn::Base64 is handled" in {
    val exp =
      """
        |{
        |  "Logical ID": {
        |    "Type": "AWS::Test::TestResource",
        |    "Properties": {
        |      "foo": { "Fn::Base64": "bar" }
        |    }
        |  }
        |}
      """.stripMargin
    val expJson = parse(exp)

    val actual = TestResource(
      logicalId = "Logical ID", foo = FnBase64(Lit("bar"))
    ).asJson

    Right(actual) should ===(expJson)
  }

  "Condition functions are handled" in {
    val exp =
      """
        |{
        |  "Logical ID" : {
        |    "Type" : "AWS::Test::TestResource",
        |    "Properties" : {
        |      "foo" : {
        |        "Fn::If" : [
        |          {
        |            "Fn::Equals" : [
        |              {
        |                "Fn::And" : [
        |                  true,
        |                  {
        |                    "Fn::Not" : [
        |                      true
        |                    ]
        |                  }
        |                ]
        |              },
        |              {
        |                "Fn::Or" : [
        |                  true,
        |                  false
        |                ]
        |              }
        |            ]
        |          },
        |          "a",
        |          "b"
        |        ]
        |      }
        |    }
        |  }
        |}
      """.stripMargin
    val expJson = parse(exp)

    val actual = TestResource(
      logicalId = "Logical ID", foo = fnIf(
        fnEquals(
          fnAnd(
            lit(true),
            fnNot(
              lit(true)
            )
          ),
          fnOr(lit(true), lit(false))
        ),
        lit("a"),
        lit("b")
      )
    ).asJson

    Right(actual) should ===(expJson)
  }

  "FindInMap function is handled" in {
    val m = Template.Mapping(
      "mapping", Map("eu-west-1" -> Map("X" -> "value"))
    )

    val actual = TestResource(
      logicalId = "ID", foo = fnFindInMap(m, PseudoParameter.Region.ref, lit("X"))
    ).asJson

    val expJson = parse(
      """
        |{
        |  "ID" : {
        |    "Type" : "AWS::Test::TestResource",
        |    "Properties" : {
        |      "foo" : {
        |        "Fn::FindInMap" : [
        |          "mapping",
        |          {"Ref": "AWS::Region"},
        |          "X"
        |        ]
        |      }
        |    }
        |  }
        |}
      """.stripMargin)

    Right(actual) should ===(expJson)
  }

  "Join function is handled" in {
     val tuple = (
      PseudoParameter.AccountId.ref, PseudoParameter.StackName.ref, lit(1)
    )

    val actual = TestResource(
      logicalId = "ID", foo = FnJoin("/", tuple)
    ).asJson

    val expJson = parse(
      """
        |{
        |  "ID" : {
        |    "Type" : "AWS::Test::TestResource",
        |    "Properties" : {
        |      "foo" : {
        |        "Fn::Join" : ["/", [
        |          {"Ref": "AWS::AccountId"},
        |          {"Ref": "AWS::StackName"},
        |          1]
        |        ]
        |      }
        |    }
        |  }
        |}
      """.stripMargin)

    Right(actual) should === (expJson)
  }

  "FnSelect is handled" in {
    val exp = fnSelect(2, lit(List("foo", "bar")))
    val expJson = parse(
      """
        |{
        |  "Fn::Select" : [
        |    2,
        |    [
        |     "foo",
        |     "bar"
        |    ]
        |  ]
        |}
      """.stripMargin)
    Right(exp.asJson) should ===(expJson)
  }

  "FnSub is handled" in {
    val varNameParam = Parameter.Str("myParam")
    val exp = fnSub("Hello ${x}", mappings = Some(Map("x" -> varNameParam.ref)))

    val expJson = parse("""
        |{
        |  "Fn::Sub" : [
        |    "Hello ${x}",
        |    {"x": { "Ref": "myParam" }}
        |  ]
        |}
      """.stripMargin)

    Right(exp.asJson) should === (expJson)
  }

  "FnSplit is handled" in {
    val exp = fnSelect(2, fnSplit("/", lit("foo/bar/baz")))

    val expJson = parse(
      """
        |{
        |  "Fn::Select" : [
        |    2,
        |    { "Fn::Split" : ["/", "foo/bar/baz"] }
        |  ]
        |}
      """.stripMargin)

    Right(exp.asJson) should === (expJson)
  }

  "FnGetAZs is supported" in {
    val exp = fnSelect(2, fnGetAZs(Some("eu-west-1")))
    val expJson = parse(
      """
        |{
        |  "Fn::Select" : [
        |    2,
        |    { "Fn::GetAZs" : "eu-west-1" }
        |  ]
        |}
      """.stripMargin)

    Right(exp.asJson) should === (expJson)
  }

  "Resource Ref is handled" in {
    val resource = TestResource("referenced", Lit("foo"))

    val actual = TestResource("ID", ResourceRef(resource)).asJson

    val expJson = parse(
      """
        |{
        |  "ID" : {
        |    "Type" : "AWS::Test::TestResource",
        |    "Properties" : {
        |      "foo" : {
        |        "Ref" : "referenced"
        |      }
        |    }
        |  }
        |}
      """.stripMargin)

    Right(actual) should ===(expJson)
  }

  "Pseudo Parameter Ref is handled" in {
    val actual = TestResource("ID", PseudoParameterRef(PseudoParameter.AccountId)).asJson

    val expJson = parse(
      """
        |{
        |  "ID" : {
        |    "Type" : "AWS::Test::TestResource",
        |    "Properties" : {
        |      "foo" : {
        |        "Ref" : "AWS::AccountId"
        |      }
        |    }
        |  }
        |}
      """.stripMargin)

    Right(actual) should ===(expJson)
  }

  "FnGetAtt" - {
    val res = TestResource(logicalId = "ID", foo = lit("bar"))

    //Note: These implicit values are created at macro expansion time
    implicit val testResourceAttr1 = HasGetAtt.mk[TestResource]("attr1")
    implicit val testResourceAttr2 = HasGetAtt.mk[TestResource]("attr2")

    "statically checks attribute names" in {
      assertCompiles("""FnGetAtt(res, "attr1")""")
      assertCompiles("""FnGetAtt(res, "attr2")""")
      assertDoesNotCompile("""FnGetAtt(res, "attr3")""")
    }
    "is handled" in {
      val expJson = parse("""{ "Fn::GetAtt": ["ID", "attr1"] } """)
      val exp: CfExp[String] = FnGetAtt(res, "attr1")
      Right(exp.asJson) should ===(expJson)
    }
  }
}
