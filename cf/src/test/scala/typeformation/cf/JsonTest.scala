package typeformation.cf

import io.circe.{Encoder, Json}
import io.circe.syntax._

trait JsonTest {
  import Encoding.dropNull

  def encode[T: Encoder](t: T): Json =
    (dropNull)(t.asJson)
}
