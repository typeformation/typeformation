package typeformation.cf.init

case class User(username: String,
                uid: Option[Int] = None,
                groups: List[String],
                homeDir: Option[String] = None)
