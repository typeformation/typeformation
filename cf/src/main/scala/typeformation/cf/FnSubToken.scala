package typeformation.cf

import typeformation.cf.CfExp.IsLit

trait FnSubToken {
  protected def showRaw: String
  protected def needsQuotes: Boolean = true

  def show: String =
    if (needsQuotes) "${" ++ showRaw ++ "}" else showRaw
}

object FnSubToken {
  case class Literal[T: IsLit](value: T) extends FnSubToken {
    override def showRaw = value.toString
    override def needsQuotes = false
  }

  case class LogicalId[T <: HasLogicalId](value: T) extends FnSubToken {
    override def showRaw = value.logicalId
  }

  case class ResourceAttribute[T <: Resource](resource: T, attr: String) extends FnSubToken {
    override def showRaw = Seq(resource.logicalId, attr).mkString(".")
  }
}
