package typeformation.cf.cloudinit

import enum.Enum

sealed trait FileEncoding
case object plain extends FileEncoding
case object base64 extends FileEncoding

object FileEncoding {
  implicit val encodingEnum: Enum[FileEncoding] = Enum.derived[FileEncoding]
}
