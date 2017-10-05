package typeformation.cf.iam

import java.util.UUID

case class Policy(Id: Option[UUID] = None,
                  Version: Version = `2012-10-17`,
                  Statement: List[Statement])
