package typeformation.cf.iam

import java.time._
import java.util.UUID

import typeformation.cf.{Arn, JsonTest}
import io.circe.parser.parse
import typeformation.cf.Encoding._
import io.circe.syntax._
import typeformation.cf.iam._
import typeformation.cf.syntax._
import Condition._

class IamEncodingTest extends JsonTest {
  "IAM entities encoding" - {
    "Principal is supported " in {
      val awsPrincipal: Principal = Principal.Aws(
        List(
          Arn("arn:aws:iam::111122223333:root"),
          Arn("arn:aws:iam::444455556666:root")
        ))

      Right(awsPrincipal.asJson) should ===(
        parse("""
        |{"AWS": ["arn:aws:iam::111122223333:root","arn:aws:iam::444455556666:root"]}
      """.stripMargin))

      val oneItemAwsPrincipal: Principal =
        Principal.Aws(List(Arn("arn:aws:iam::111122223333:root")))

      Right(oneItemAwsPrincipal.asJson) should ===(
        parse("""
        |{"AWS": "arn:aws:iam::111122223333:root"}
      """.stripMargin))

      val servicesPrincipal: Principal =
        Principal.Service(
          List("ec2.amazonaws.com", "datapipeline.amazonaws.com"))

      Right(servicesPrincipal.asJson) should ===(
        parse("""{
              |  "Service": [
              |    "ec2.amazonaws.com",
              |    "datapipeline.amazonaws.com"
              |  ]}
      """.stripMargin))

      val canonicalUser: Principal =
        Principal.CanonicalUser(
          "79a59df900b949e55d96a1e698fbacedfd6e09d98eacf8f8d5218e7cd47ef2be")

      Right(canonicalUser.asJson) should ===(
        parse(
          """{ "CanonicalUser": "79a59df900b949e55d96a1e698fbacedfd6e09d98eacf8f8d5218e7cd47ef2be" }"""
        ))
    }
    "Conditions are supported" - {
      "supports positive conditions" in {
        val condition1: Condition =
          StringEquals(Key("aws:UserAgent"), List("Example Corp Java Client"))

        Right(condition1.asJson) should ===(parse(
          """{"StringEquals": {"aws:UserAgent": "Example Corp Java Client"}}"""))

        val condition2: Condition =
          IpAddress(Key("aws:SourceIp"), List("192.0.2.0/24", "203.0.113.0/24"))

        Right(condition2.asJson) should ===(
          parse("""{
          |  "IpAddress" : {
          |    "aws:SourceIp" : ["192.0.2.0/24", "203.0.113.0/24"]
          |  }
          |}
          """.stripMargin))

        val condition3: Condition =
          DateLessThan(Key("aws:CurrentTime"),
                       expected =
                         ZonedDateTime.of(LocalDateTime.of(2013, 6, 30, 0, 0),
                                          ZoneOffset.UTC))

        Right(condition3.asJson) should ===(
          parse("""
            |{"DateLessThan": {"aws:CurrentTime": "2013-06-30T00:00:00Z"}}
          """.stripMargin))
      }
      "Support qualifiers" in {
        Seq[Qualifier](ForAnyValue, ForAllValues)
          .foreach { qualifier =>
            val condition: Condition =
              StringEquals(Key("aws:UserAgent"),
                           List("IE6", "IE7"),
                           Some(qualifier))

            Right(condition.asJson) should ===(parse(
              s"""{"${qualifier.id}:StringEquals": {"aws:UserAgent": ["IE6", "IE7"]}}"""))
          }

      }
      "Support ifExist" in {
        val condition: Condition =
          StringEquals(key = Key("ec2:InstanceType"),
                       expected = List("t1.*"),
                       ifExists = true)

        Right(condition.asJson) should ===(
          parse(s"""{"StringEqualsIfExists": {"ec2:InstanceType": "t1.*"}}"""))
      }
    }
    "Policies are supported" in {
      val policy =
        Policy(
          Id = Some(UUID.fromString("cd3ad3d9-2776-4ef1-a904-4c229d1642ee")),
          Statement = List(
            Statement(
              Sid = Some("bucket-get-put"),
              Effect = Effect.Allow,
              Principal = Some(Principal.CanonicalUser("user-id")),
              Resource = List(Arn("arn:aws:s3:::mybucket")),
              Condition = List(
                StringLike(Key("s3:prefix"), List("${aws:username}/*")),
                StringNotLike(Key("s3:prefix"), List("secrets/*")),
              ),
              Action =
                Invertible.Pos(Action(Set("s3:GetObject", "s3:PutObject")))
            ))
        )

      Right(policy.asJson) should ===(
        parse("""
        | {
        |   "Id": "cd3ad3d9-2776-4ef1-a904-4c229d1642ee",
        |   "Version": "2012-10-17",
        |   "Statement": [
        |      {
        |          "Sid": "bucket-get-put",
        |          "Effect": "Allow",
        |          "Resource": "arn:aws:s3:::mybucket",
        |          "Action": [
        |              "s3:GetObject",
        |              "s3:PutObject"
        |          ],
        |          "Condition": {
        |            "StringNotLike": { "s3:prefix": "secrets/*" },
        |            "StringLike": { "s3:prefix" : "${aws:username}/*" }
        |          },
        |          "Principal": {
        |            "CanonicalUser": "user-id"
        |          }
        |      }
        |   ]
        | }""".stripMargin))
    }
  }
}
