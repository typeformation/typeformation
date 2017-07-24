package typeformation.cf.init

import typeformation.cf.syntax.lit
import typeformation.cf.{CfExp, HasLogicalId}

object Init {
  case class ConfigSet(logicalId: String, configs: List[Config]) extends HasLogicalId
  case class Config(logicalId: String,
                    commands: List[Command] = Nil,
                    files: List[File] = Nil,
                    groups: List[Group] = Nil,
                    packages: List[Package] = Nil,
                    services: Service.Services = Service.Sysvinit(Nil),
                    sources: List[Source] = Nil,
                    users: List[User] = Nil) extends HasLogicalId

  def apply(config: Config): Init =
    Init(List(ConfigSet(config.logicalId, List(config))))

  def apply(sets: List[ConfigSet]): Init = {
    val configs = sets.flatMap(_.configs)
    val configSets = sets.map { s => s.logicalId -> lit(s.configs.map(_.logicalId)) }.toMap
    Init(configSets, configs.toSet)
  }
}
case class Init(configSets: Map[String, CfExp[List[String]]], configs: Set[Init.Config])
