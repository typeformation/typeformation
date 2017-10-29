package typeformation.iam.syntax
import java.time.ZonedDateTime

import io.circe.Encoder
import typeformation.cf.Arn
import typeformation.iam.Condition
import typeformation.iam.Condition.Key

trait Conditions {
  private def strListCond(implicit ev: Encoder[List[String]]) =
    Condition.instance[List[String]](None, ifX = false)(
      _: Key,
      _: List[String])(_: String)

  private def strCond(implicit ev: Encoder[String]) =
    Condition.instance[String](None, ifX = false)(_: Key, _: String)(_: String)

  private def dateTimeCond(implicit ev: Encoder[ZonedDateTime]) =
    Condition.instance[ZonedDateTime](None, ifX = false)(
      _: Key,
      _: ZonedDateTime)(_: String)

  private def numericCond[N](implicit enc: Encoder[N], isNumeric: Numeric[N]) =
    Condition.instance[N](None, ifX = false)(_: Key, _: N)(_: String)

  private def arnCond(implicit enc: Encoder[Arn]) =
    Condition.instance[Arn](None, ifX = false)(_: Key, _: Arn)(_: String)

  def stringEquals(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]): Condition =
    strListCond(enc)(key, exp, "StringEquals")

  def stringNotEquals(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]): Condition =
    strListCond(enc)(key, exp, "StringNotEquals")

  def stringEqualsIgnoreCase(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]): Condition =
    strListCond(enc)(key, exp, "StringEqualsIgnoreCase")

  def stringNotEqualsIgnoreCase(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]): Condition =
    strListCond(enc)(key, exp, "StringNotEqualsIgnoreCase")

  def stringLike(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]): Condition =
    strListCond(enc)(key, exp, "StringLike")

  def stringNotLike(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]): Condition =
    strListCond(enc)(key, exp, "StringNotLike")

  def numericEquals[N: Numeric](key: Key, exp: N)(
      implicit enc: Encoder[N],
      isNum: Numeric[N]): Condition =
    numericCond(enc, isNum)(key, exp, "NumericEquals")

  def numericNotEquals[N: Numeric](key: Key, exp: N)(
      implicit enc: Encoder[N],
      isNum: Numeric[N]): Condition =
    numericCond(enc, isNum)(key, exp, "NumericNotEquals")

  def numericLessThan[N: Numeric](key: Key, exp: N)(
      implicit enc: Encoder[N],
      isNum: Numeric[N]): Condition =
    numericCond(enc, isNum)(key, exp, "NumericLessThan")

  def numericLessThanEquals[N: Numeric](key: Key, exp: N)(
      implicit enc: Encoder[N],
      isNum: Numeric[N]): Condition =
    numericCond(enc, isNum)(key, exp, "NumericLessThanEquals")

  def numericGreaterThan[N: Numeric](key: Key, exp: N)(
      implicit enc: Encoder[N],
      isNum: Numeric[N]): Condition =
    numericCond(enc, isNum)(key, exp, "NumericGreaterThan")

  def numericGreaterThanEquals[N: Numeric](key: Key, exp: N)(
      implicit enc: Encoder[N],
      isNum: Numeric[N]): Condition =
    numericCond(enc, isNum)(key, exp, "NumericGreaterThanEquals")

  def dateEquals(key: Key, exp: ZonedDateTime)(
      implicit enc: Encoder[ZonedDateTime]): Condition =
    dateTimeCond(enc)(key, exp, "DateEquals")

  def dateNotEquals(key: Key, exp: ZonedDateTime)(
      implicit enc: Encoder[ZonedDateTime]): Condition =
    dateTimeCond(enc)(key, exp, "DateNotEquals")

  def dateLessThan(key: Key, exp: ZonedDateTime)(
      implicit enc: Encoder[ZonedDateTime]): Condition =
    dateTimeCond(enc)(key, exp, "DateLessThan")

  def dateLessThanEquals(key: Key, exp: ZonedDateTime)(
      implicit enc: Encoder[ZonedDateTime]): Condition =
    dateTimeCond(enc)(key, exp, "DateLessThanEquals")

  def dateGreaterThan(key: Key, exp: ZonedDateTime)(
      implicit enc: Encoder[ZonedDateTime]): Condition =
    dateTimeCond(enc)(key, exp, "DateGreaterThan")

  def dateGreaterThanEquals(key: Key, exp: ZonedDateTime)(
      implicit enc: Encoder[ZonedDateTime]): Condition =
    dateTimeCond(enc)(key, exp, "DateGreaterThanEquals")

  def bool(key: Key, exp: Boolean)(implicit enc: Encoder[Boolean]) =
    Condition.instance[Boolean](None, ifX = false)(key, exp)("Boolean")

  def binaryEquals(key: Key, exp: String)(implicit enc: Encoder[String]) =
    strCond(enc)(key, exp, "BinaryEquals")

  def ipAddress(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]) =
    strListCond(enc)(key, exp, "IpAddress")

  def notIpAddress(key: Key, exp: List[String])(
      implicit enc: Encoder[List[String]]) =
    strListCond(enc)(key, exp, "NotIpAddress")

  def arnEquals(key: Key, exp: Arn)(implicit enc: Encoder[Arn]) =
    arnCond(enc)(key, exp, "ArnEquals")

  def arnNotEquals(key: Key, exp: Arn)(implicit enc: Encoder[Arn]) =
    arnCond(enc)(key, exp, "ArnNotEquals")

  def arnLike(key: Key, exp: Arn)(implicit enc: Encoder[Arn]) =
    arnCond(enc)(key, exp, "ArnLike")

  def arnNotLike(key: Key, exp: Arn)(implicit enc: Encoder[Arn]) =
    arnCond(enc)(key, exp, "ArnNotLike")

  def isNull(key: Key, exp: Boolean)(implicit enc: Encoder[Boolean]) =
    Condition.instance[Boolean](None, ifX = false)(key, exp)("Null")
}
