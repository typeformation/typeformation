package typeformation.cf

import enum.Enum

package object iam {
  case class Action(value: String*)

  sealed trait Effect
  object Effect {
    object Allow extends Effect
    object Deny extends Effect
    val effectEnum: Enum[Effect] = Enum.derived[Effect]
  }

  sealed trait Version
  object `2012-10-17` extends Version

  object Version {
    implicit val versionEnum: Enum[Version] = Enum.derived[Version]
  }
}
