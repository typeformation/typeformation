package typeformation.cf

import shapeless.Witness

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "${T} is not a known property of resource ${R}. Please review docs http://amzn.to/1hHXfM4, and consider using FnGetAtt.unsafe if needed.")
trait ResourceAtt[R <: Resource, T] {
  def attributeName: String
}

object ResourceAtt {
  def mk[R <: Resource](w: Witness.Lt[String]): ResourceAtt[R, w.T] = new ResourceAtt[R, w.T] {
    override val attributeName = w.value
  }

  def apply[R <: Resource](w: Witness)(
    implicit
    ev: ResourceAtt[R, w.T]): ResourceAtt[R, w.T] = ev
}
