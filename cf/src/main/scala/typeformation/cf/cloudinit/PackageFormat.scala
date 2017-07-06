package typeformation.cf.cloudinit

import enum.Enum

sealed trait PackageFormat
case object apt extends PackageFormat
case object rpm extends PackageFormat
case object msi extends PackageFormat
case object python extends PackageFormat
case object rubygems extends PackageFormat
case object yum extends PackageFormat

object PackageFormat {
  implicit val formatEnum: Enum[PackageFormat] = Enum.derived[PackageFormat]
}
