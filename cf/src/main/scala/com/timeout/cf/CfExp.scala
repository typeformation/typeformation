package com.timeout.cf

import java.time.{Duration, ZonedDateTime}

import com.timeout.cf.Template.Mapping
import io.circe.{Encoder, Json}
import io.circe.syntax._
import shapeless.Witness

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

  def lit[T: IsLit](t: T): CfExp[T] = Lit(t)

  private [cf] case class ResourceRef(value: Resource) extends Ref[Resource] with E[String]
  private [cf] case class ParameterRef(value: Parameter) extends Ref[Parameter] with E[String]
  private [cf] case class PseudoParameterRef(value: PseudoParameter) extends Ref[PseudoParameter] with E[String]

  def ref(res: Resource): E[String] with Ref[Resource] = ResourceRef(res)
  def ref(res: Parameter): E[String] with Ref[Parameter] = ParameterRef(res)
  def ref(res: PseudoParameter): E[String] with Ref[PseudoParameter] = PseudoParameterRef(res)

  private [cf] case class FnBase64(exp: E[String]) extends E[String]
  def fnBase64(exp: E[String]): E[String] = FnBase64(exp)

  private [cf] case class FnAnd(cond1: E[Boolean], cond2: E[Boolean]) extends E[Boolean]
  def fnAnd(cond1: E[Boolean], cond2: E[Boolean]): E[Boolean] = FnAnd(cond1, cond2)

  private [cf] case class FnEquals[T](left: E[T], right: E[T]) extends E[Boolean]
  def fnEquals[T](l: E[T], r: E[T]): E[Boolean] = FnEquals(l, r)

  private [cf] case class FnIf[T](cond: E[Boolean], ifTrue: E[T], ifFalse: E[T]) extends E[T]
  def fnIf[T](cond: E[Boolean], ifTrue: E[T], ifFalse: E[T]): E[T] = FnIf(cond, ifTrue, ifFalse)

  private [cf] case class FnNot(cond: E[Boolean]) extends E[Boolean]
  def fnNot(cond: E[Boolean]): E[Boolean] = FnNot(cond)

  private [cf] case class FnOr(conds: Seq[E[Boolean]]) extends E[Boolean]
  def fnOr(conds: E[Boolean]*) = FnOr(conds)

  private [cf] case class FnSelect[T](index: Int, values: E[List[T]]) extends E[T]
  def fnSelect[T](index: Int, values: E[List[T]]): E[T] = FnSelect(index, values)

  private [cf] case class FnGetAZs(region: Option[String]) extends E[List[String]]
  def fnGetAZs(region: Option[String] = None): E[List[String]] = FnGetAZs(region)

  /** We might want to implement this as a type-safe string interpolator checking that
    * the variable names match either a pseudo param (e.g. AWS::Region)
    * or one of the supplied mappings
    */
  private [cf] case class FnSub(string: String,
                                mappings: Option[Map[String,
                                                 CfExp[String]]]) extends E[String]
  def fnSub(s: String, mappings: Option[Map[String, CfExp[String]]] = None): E[String] = FnSub(s, mappings)

  private [cf] case class FnGetAtt(logicalId: String, attributeName: String) extends E[String]
  object FnGetAtt {
    def unsafe(v: HasLogicalId, attributeName: String): E[String] = FnGetAtt(v.logicalId, attributeName)
    def apply[R <: Resource](resource: R, w: Witness)
                            (implicit hasGetAtt: HasGetAtt[R, w.T]): FnGetAtt =
      FnGetAtt(resource.logicalId, hasGetAtt.attributeName)
  }

  private [cf] case class FnFindInMap(mapName: Mapping,
                                      topLevelKey: E[String],
                                      secondLevelKey: E[String]) extends E[String]
  def fnFindInMap(mapping: Mapping, topLevelKey: E[String], secondLevelKey: E[String]): E[String] =
    FnFindInMap(mapping, topLevelKey, secondLevelKey)

  private [cf] case class FnSplit(delimiter: String, string: E[String]) extends E[List[String]]

  def fnSplit(delimiter: String, string: E[String]): E[List[String]] =
    FnSplit(delimiter, string)

  private [cf] case class FnJoin(delimiter: String, values: List[Json]) extends E[String]
  object FnJoin {
    import shapeless._
    import shapeless.ops.hlist._
    import Encoding._

    object encodeExp extends Poly1 {
      implicit def exp2Json[A: Encoder : IsLit] = at[CfExp[A]](_.asJson)
    }

    def apply[T <: Product, L1 <: HList, L2 <: HList](separator: String, tuple: T)(
      implicit
      gen: Generic.Aux[T, L1],
      mapper: Mapper.Aux[encodeExp.type, L1, L2],
      traversable: ToTraversable.Aux[L2, List, Json])
    : CfExp[String] =
      FnJoin(separator, gen.to(tuple).map(encodeExp).toList)
  }
}
