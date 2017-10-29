package typeformation.cf

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{FreeSpec, Matchers}

trait JsonTest extends FreeSpec with Matchers with TypeCheckedTripleEquals {
  import Encoding.dropNull

  def encode[T: Encoder](t: T): Json =
    (dropNull)(t.asJson)
}
