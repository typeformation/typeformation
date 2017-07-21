package typeformation.cf

trait FnSubToken {
  def show: String
}

object FnSubToken {
  case class LogicalId[T <: HasLogicalId](value: T) extends FnSubToken {
    override def show = value.logicalId
  }

  case class ResourceAttribute[T <: Resource](resource: T, attr: String) extends FnSubToken {
    override def show = Seq(resource.logicalId, attr).mkString(".")
  }
}
