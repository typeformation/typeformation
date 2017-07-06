package typeformation.cf.cloudinit

import typeformation.cf.HasLogicalId

case class Group(logicalId: String, gid: Option[Int]) extends HasLogicalId
