package typeformation.iam

import io.circe.Encoder

trait Condition {
  type Type
  def key: Condition.Key
  def expected: Type
  def quantifier: Option[Condition.Quantifier]
  def hasIfExists: Boolean
  def label: String
  def expressionEncoder: Encoder[Type]

  def ifExists: Condition =
    Condition.instance[Type](quantifier, ifX = true)(key, expected)(label)(
      expressionEncoder)

  def forAllValues: Condition =
    Condition.instance[Type](Some(Condition.ForAllValues), hasIfExists)(
      key,
      expected)(label)(expressionEncoder)

  def forAnyValues: Condition =
    Condition.instance[Type](Some(Condition.ForAnyValue), hasIfExists)(
      key,
      expected)(label)(expressionEncoder)
}

object Condition {
  case class Key(value: String)

  private[iam] def instance[A: Encoder](q: Option[Quantifier], ifX: Boolean)(
      k: Key,
      exp: A)(lbl: String): Condition { type Type = A } = new Condition {
    override type Type = A
    override def key = k
    override val expected = exp
    override val label = lbl
    override val quantifier: Option[Quantifier] = q
    override val hasIfExists: Boolean = ifX
    override val expressionEncoder: Encoder[A] = implicitly[Encoder[A]]
  }

  trait Quantifier {
    def id: String
  }
  case object ForAnyValue extends Quantifier {
    override def id = "ForAnyValues"
  }
  case object ForAllValues extends Quantifier {
    override def id = "ForAllValues"
  }
}
