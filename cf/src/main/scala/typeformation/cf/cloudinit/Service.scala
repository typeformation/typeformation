package typeformation.cf.cloudinit

object Service {
  sealed trait Services {
    def toList: List[Service]
  }
  case class Windows(toList: List[Service]) extends Services
  case class Sysvinit(toList: List[Service]) extends Services
}
case class Service(name: String,
                   enabled: Boolean,
                   ensureRunning: Boolean,
                   files: List[String] = Nil,
                   sources: List[String] = Nil,
                   packages: Map[PackageFormat, List[String]] = Map.empty,
                   commands: List[String] = Nil)