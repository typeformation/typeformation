package typeformation.cf.init

import PackageFormat._

case class Package(name: String,
                   format: PackageFormat,
                   version: List[String] = Nil)