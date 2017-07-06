package typeformation.cf.cloudinit

case class User(username: String,
                uid: Option[Int] = None,
                groups: List[String],
                homeDir: Option[String] = None)
