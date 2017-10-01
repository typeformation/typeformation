package typeformation.cf.syntax

import typeformation.cf._
import typeformation.cf.CfExp._
import io.circe.syntax._
import io.circe.{Encoder, Json}
import shapeless.{Generic, HList, Poly1, Witness}
import shapeless.ops.hlist._
import Encoding._

trait Functions {
  def lit[T: IsLit](t: T): CfExp[T] = Lit(t)

  def ref(res: Resource): E[String] with Ref[Resource] = ResourceRef(res)
  def ref(res: Parameter): E[String] with Ref[Parameter] = ParameterRef(res)
  def ref(res: PseudoParameter): E[String] with Ref[PseudoParameter] = PseudoParameterRef(res)

  def fnBase64(exp: E[String]): E[String] = FnBase64(exp)

  def fnAnd(cond1: E[Boolean], cond2: E[Boolean]): E[Boolean] = FnAnd(cond1, cond2)

  def fnOr(conds: E[Boolean]*) = FnOr(conds)

  def fnNot(cond: E[Boolean]): E[Boolean] = FnNot(cond)

  def fnEquals[T](l: E[T], r: E[T]): E[Boolean] = FnEquals(l, r)

  def fnIf[T](cond: E[Boolean], ifTrue: E[T], ifFalse: E[T]): E[T] = FnIf(cond, ifTrue, ifFalse)

  def fnSelect[T](index: Int, values: E[List[T]]): E[T] = FnSelect(index, values)

  def fnGetAZs(region: Option[String] = None): E[List[String]] = FnGetAZs(region)

  def fnSub(s: String, mappings: Option[Map[String, CfExp[String]]] = None): E[String] = FnSub(s, mappings)

  def fnFindInMap(mapping: Template.Mapping, topLevelKey: E[String], secondLevelKey: E[String]): E[String] =
    FnFindInMap(mapping, topLevelKey, secondLevelKey)

  def fnSplit(delimiter: String, string: E[String]): E[List[String]] =
    FnSplit(delimiter, string)

  def fnGetAtUnsafe(v: HasLogicalId, attributeName: String): E[String] =
    FnGetAtt(v.logicalId, attributeName)

  def fnGetAtt[R <: Resource](resource: R, w: Witness)
                             (implicit hasGetAtt: ResourceAtt[R, w.T]): E[String] =
      FnGetAtt(resource.logicalId, hasGetAtt.attributeName)

  private object encodeExp extends Poly1 {
    implicit def exp2Json[A: Encoder : IsLit] = at[CfExp[A]](_.asJson)
  }

  def fnJoin[T <: Product, L1 <: HList, L2 <: HList](separator: String, tuple: T)(
      implicit
      gen: Generic.Aux[T, L1],
      mapper: Mapper.Aux[encodeExp.type, L1, L2],
      traversable: ToTraversable.Aux[L2, List, Json])
    : CfExp[String] =
      FnJoin(separator, gen.to(tuple).map(encodeExp).toList)

  def fnImportValue(sharedValue: E[String]): E[String] = FnImportValue(sharedValue)
  def fnImportValue(output: Output): Option[E[String]] = output.Export.map(FnImportValue)
}
