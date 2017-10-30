package typeformation.iam
import typeformation.cf.Arn
import typeformation.iam.Invertible.Pos

case class Statement(Effect: Effect,
                     Action: Invertible[Action],
                     Resource: Invertible[List[Arn]] = Pos(Nil),
                     Condition: List[Condition] = Nil,
                     Principal: Option[Invertible[Principal]] = None,
                     Sid: Option[String] = None)
