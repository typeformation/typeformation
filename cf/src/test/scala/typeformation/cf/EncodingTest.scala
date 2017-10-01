package typeformation.cf

import io.circe.Json
import org.scalatest.{FreeSpec, Matchers}
import typeformation.cf.Encoding.{dropNull, emptyToNull}
import io.circe.parser.parse

class EncodingTest extends FreeSpec with Matchers {
  "dropNull" - {
    "removes null keys from flat objects" in {
      Right(dropNull(Json.obj(
        "a" -> Json.fromInt(1),
        "b" -> Json.Null,
        "c" -> Json.obj()
      ))) should ===(parse(
        """
          |{
          |  "a": 1,
          |  "c": {}
          |}
        """.stripMargin))
    }

    "removes null keys from nested structures" in {
      Right(dropNull(Json.obj(
        "a" -> Json.fromInt(1),
        "b" -> Json.Null,
        "c" -> Json.obj(
          "c1" -> Json.arr(Json.Null, Json.fromInt(1), Json.obj("c1a" -> Json.obj()))
        )
      ))) should ===(parse(
        """
          |{
          |  "a": 1,
          |  "c": {
          |    "c1": [
          |      1,
          |      { "c1a": {}}
          |    ]
          |  }
          |}
        """.stripMargin))
    }
  }

}
