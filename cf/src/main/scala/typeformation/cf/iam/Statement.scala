package typeformation.cf.iam
import typeformation.cf.Arn

case class Statement(Effect: Effect,
                     Action: Invertible[Action],
                     Resource: Invertible[List[Arn]],
                     Condition: List[Condition] = Nil,
                     Principal: Option[Invertible[Principal]] = None,
                     Sid: Option[String] = None)
