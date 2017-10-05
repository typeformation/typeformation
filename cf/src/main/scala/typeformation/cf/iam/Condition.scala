package typeformation.cf.iam

import java.time.ZonedDateTime
import typeformation.cf.{Arn, CfExp}

sealed trait Condition { self: Product =>
  type Type
  def key: Condition.Key
  def expected: Type
  def qualifier: Option[Condition.Qualifier]
  def ifExists: Boolean
  def label: String = productPrefix
}

object Condition {
  case class Key(value: String)

  trait CfNumeric[T]
  object CfNumeric {
    implicit val numericInt: CfNumeric[Int] = new CfNumeric[Int] {}
    implicit val numericFloat: CfNumeric[Float] = new CfNumeric[Float] {}
  }

  trait Qualifier {
    def id: String
  }

  case object ForAnyValue extends Qualifier {
    override def id = "ForAnyValues"
  }

  case object ForAllValues extends Qualifier {
    override def id = "ForAllValues"
  }

  trait StrCondition extends Condition { self: Product =>
    override type Type = CfExp[String]
  }

  trait StrSeqCondition extends Condition { self: Product =>
    override type Type = CfExp[List[String]]
  }

  trait NumericCondition[T] extends Condition { self: Product =>
    override type Type = T
  }

  trait DateTimeCondition extends Condition { self: Product =>
    override type Type = ZonedDateTime
  }

  trait BoolCondition extends Condition { self: Product =>
    override type Type = Boolean
  }

  trait ArnCondition extends Condition { self: Product =>
    override type Type = Arn
  }

  case class StringEquals(key: Condition.Key,
                          expected: CfExp[List[String]],
                          qualifier: Option[Qualifier] = None,
                          ifExists: Boolean = false)
      extends StrSeqCondition {}

  case class StringNotEquals(key: Condition.Key,
                             expected: CfExp[List[String]],
                             qualifier: Option[Qualifier] = None,
                             ifExists: Boolean = false)
      extends StrSeqCondition {}

  case class StringEqualsIgnoreCase(key: Condition.Key,
                                    expected: CfExp[List[String]],
                                    qualifier: Option[Qualifier] = None,
                                    ifExists: Boolean = false)
      extends StrSeqCondition {}

  case class StringNotEqualsIgnoreCase(key: Condition.Key,
                                       expected: CfExp[List[String]],
                                       qualifier: Option[Qualifier] = None,
                                       ifExists: Boolean = false)
      extends StrSeqCondition {}

  case class StringLike(key: Condition.Key,
                        expected: CfExp[List[String]],
                        qualifier: Option[Qualifier] = None,
                        ifExists: Boolean = false)
      extends StrSeqCondition {}

  case class StringNotLike(key: Condition.Key,
                           expected: CfExp[List[String]],
                           qualifier: Option[Qualifier] = None,
                           ifExists: Boolean = false)
      extends StrSeqCondition {}

  case class NumericEquals[T: CfNumeric](key: Condition.Key,
                                         expected: T,
                                         qualifier: Option[Qualifier] = None,
                                         ifExists: Boolean = false)
      extends NumericCondition[T] {}

  case class NumericNotEquals[T: CfNumeric](key: Condition.Key,
                                            expected: T,
                                            qualifier: Option[Qualifier] = None,
                                            ifExists: Boolean = false)
      extends NumericCondition[T] {}

  case class NumericLessThen[T: CfNumeric](key: Condition.Key,
                                           expected: T,
                                           qualifier: Option[Qualifier] = None,
                                           ifExists: Boolean = false)
      extends NumericCondition[T] {}

  case class NumericGreaterThen[T: CfNumeric](key: Condition.Key,
                                              expected: T,
                                              qualifier: Option[Qualifier] =
                                                None,
                                              ifExists: Boolean = false)
      extends NumericCondition[T] {}

  case class NumericLessThenEquals[T: CfNumeric](key: Condition.Key,
                                                 expected: T,
                                                 qualifier: Option[Qualifier] =
                                                   None,
                                                 ifExists: Boolean = false)
      extends NumericCondition[T] {}

  case class NumericGreaterThenEquals[T: CfNumeric](
      key: Condition.Key,
      expected: T,
      qualifier: Option[Qualifier] = None,
      ifExists: Boolean = false)
      extends NumericCondition[T] {}

  case class DateEquals(key: Condition.Key,
                        expected: ZonedDateTime,
                        qualifier: Option[Qualifier] = None,
                        ifExists: Boolean = false)
      extends DateTimeCondition {}

  case class DateNotEquals(key: Condition.Key,
                           expected: ZonedDateTime,
                           qualifier: Option[Qualifier] = None,
                           ifExists: Boolean = false)
      extends DateTimeCondition {}

  case class DateLessThan(key: Condition.Key,
                          expected: ZonedDateTime,
                          qualifier: Option[Qualifier] = None,
                          ifExists: Boolean = false)
      extends DateTimeCondition {}

  case class DateGreaterThen(key: Condition.Key,
                             expected: ZonedDateTime,
                             qualifier: Option[Qualifier] = None,
                             ifExists: Boolean = false)
      extends DateTimeCondition {}

  case class DateLessThenEquals(key: Condition.Key,
                                expected: ZonedDateTime,
                                qualifier: Option[Qualifier] = None,
                                ifExists: Boolean = false)
      extends DateTimeCondition {}

  case class DateGreaterThenEquals(key: Condition.Key,
                                   expected: ZonedDateTime,
                                   qualifier: Option[Qualifier] = None,
                                   ifExists: Boolean = false)
      extends DateTimeCondition {}

  case class Bool(key: Condition.Key,
                  expected: Boolean,
                  qualifier: Option[Qualifier] = None,
                  ifExists: Boolean = false)
      extends BoolCondition {}

  case class BinaryEquals(key: Condition.Key,
                          expected: CfExp[String],
                          qualifier: Option[Qualifier] = None,
                          ifExists: Boolean = false)
      extends StrCondition

  case class Null(key: Condition.Key,
                  expected: Boolean,
                  qualifier: Option[Qualifier])
      extends BoolCondition {
    override val ifExists = false
  }

  case class IpAddress(key: Condition.Key,
                       expected: CfExp[List[String]],
                       qualifier: Option[Qualifier] = None,
                       ifExists: Boolean = false)
      extends StrSeqCondition

  case class NotIpAddress(key: Condition.Key,
                          expected: CfExp[List[String]],
                          qualifier: Option[Qualifier] = None,
                          ifExists: Boolean = false)
      extends StrSeqCondition

  case class ArnEquals(key: Condition.Key,
                       expected: Arn,
                       qualifier: Option[Qualifier] = None,
                       ifExists: Boolean = false)
      extends ArnCondition

  case class ArnNotEquals(key: Condition.Key,
                          expected: Arn,
                          qualifier: Option[Qualifier] = None,
                          ifExists: Boolean = false)
      extends ArnCondition

  case class ArnLike(key: Condition.Key,
                     expected: Arn,
                     qualifier: Option[Qualifier] = None,
                     ifExists: Boolean = false)
      extends ArnCondition

  case class ArnNotLike(key: Condition.Key,
                        expected: Arn,
                        qualifier: Option[Qualifier] = None,
                        ifExists: Boolean = false)
      extends ArnCondition
}
