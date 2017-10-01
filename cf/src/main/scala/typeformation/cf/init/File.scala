package typeformation.cf.init

import typeformation.cf.CfExp

sealed trait File {
  def path: String
  def group: Option[String]
  def owner: Option[String]
  def mode: Option[String]
  def authentication: Option[String]
}

object File {
  case class FromString(path: String,
                        content: CfExp[String],
                        encoding: Option[FileEncoding] = None,
                        mode: Option[String] = None,
                        group: Option[String] = None,
                        owner: Option[String] = None,
                        authentication: Option[String] = None) extends File

  case class FromUrl(path: String,
                     source: CfExp[String],
                     encoding: Option[FileEncoding] = None,
                     mode: Option[String] = None,
                     group: Option[String] = None,
                     owner: Option[String] = None,
                     authentication: Option[String] = None) extends File
}


