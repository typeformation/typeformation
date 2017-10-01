package typeformation.cf

import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.Json
import io.circe.syntax._
import Encoding._
import ResourceAttributes._
import typeformation.CloudformationGen

object resources {
  @CloudformationGen()
  object Gen
}
