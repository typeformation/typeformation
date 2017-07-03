package typeformation.cf

import java.time.{Duration, ZonedDateTime}

import Template.Mapping
import io.circe.Json

trait CfExp[+T]
object CfExp {
  type E[+T] = CfExp[T]

  trait IsLit[T]

  trait Ref[T] {
    def value: T
  }

  private [cf] object IsLit {
    implicit val stringLit: IsLit[String] = new IsLit[String] {}
    implicit val IntLit: IsLit[Int] = new IsLit[Int] {}
    implicit val longLit: IsLit[Long] = new IsLit[Long] {}
    implicit val doubleLit: IsLit[Double] = new IsLit[Double] {}
    implicit val boolLit: IsLit[Boolean] = new IsLit[Boolean] {}
    implicit val dateTimeLit: IsLit[ZonedDateTime] = new IsLit[ZonedDateTime] {}
    implicit val jsonLit: IsLit[Json] = new IsLit[Json] {}
    implicit val durationLit: IsLit[Duration] = new IsLit[Duration] {}
    implicit def propertyLit[T <: ResourceProperty]: IsLit[T] = new IsLit[T] {}
    implicit def listLit[T: IsLit]: IsLit[List[T]] = new IsLit[List[T]]{}
  }

  private [cf] case class Lit[T: IsLit](value: T) extends E[T]

  private [cf] case class ResourceRef(value: Resource) extends Ref[Resource] with E[String]
  private [cf] case class ParameterRef(value: Parameter) extends Ref[Parameter] with E[String]
  private [cf] case class PseudoParameterRef(value: PseudoParameter) extends Ref[PseudoParameter] with E[String]

  private [cf] case class FnBase64(exp: E[String]) extends E[String]

  private [cf] case class FnAnd(cond1: E[Boolean], cond2: E[Boolean]) extends E[Boolean]

  private [cf] case class FnEquals[T](left: E[T], right: E[T]) extends E[Boolean]

  private [cf] case class FnIf[T](cond: E[Boolean], ifTrue: E[T], ifFalse: E[T]) extends E[T]

  private [cf] case class FnNot(cond: E[Boolean]) extends E[Boolean]

  private [cf] case class FnOr(conds: Seq[E[Boolean]]) extends E[Boolean]

  private [cf] case class FnSelect[T](index: Int, values: E[List[T]]) extends E[T]

  private [cf] case class FnGetAZs(region: Option[String]) extends E[List[String]]

  /** We might want to implement this as a type-safe string interpolator checking that
    * the variable names match either a pseudo param (e.g. AWS::Region)
    * or one of the supplied mappings
    */
  private [cf] case class FnSub(string: String,
                                mappings: Option[Map[String, CfExp[String]]]) extends E[String]

  private [cf] case class FnGetAtt(logicalId: String, attributeName: String) extends E[String]

  private [cf] case class FnFindInMap(mapName: Mapping,
                                      topLevelKey: E[String],
                                      secondLevelKey: E[String]) extends E[String]

  private [cf] case class FnSplit(delimiter: String, string: E[String]) extends E[List[String]]

  private [cf] case class FnJoin(delimiter: String, values: List[Json]) extends E[String]

  private [cf] case class FnImportValue(sharedValueToImport: E[String]) extends E[String]
}
