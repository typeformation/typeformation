package typeformation.cf.syntax

import io.circe.{Encoder, Json}
import typeformation.cf.CfExp.{FnJoin, FnSub}
import typeformation.cf.{CfExp, FnSubToken}

import scala.collection.mutable

trait Interpolators {
  implicit class FnSubInterpolator(val sc: StringContext) {
    def fnJoin[T](args: CfExp[T]*)(implicit enc: Encoder[CfExp[T]]): FnJoin = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val tokens = mutable.ListBuffer[Json](Json.fromString(strings.next()))

      while(strings.hasNext) {
        val s = strings.next
        tokens += enc(expressions.next())
        if (!s.isEmpty) tokens += Json.fromString(s)
      }

      FnJoin("", tokens.toList)
    }

    def fnSub(args: FnSubToken*): FnSub = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while(strings.hasNext) {
        buf append expressions.next.show
        buf append strings.next
      }
      FnSub(buf.toString, None)
    }
  }

}
