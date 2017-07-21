package typeformation.cf.syntax

import typeformation.cf.CfExp.FnSub
import typeformation.cf.FnSubToken

trait Interpolators {
  implicit class FnSubInterpolator(val sc: StringContext) {
    def fnSub(args: FnSubToken*): FnSub = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while(strings.hasNext) {
        buf append "${" ++ expressions.next.show ++ "}"
        buf append strings.next
      }
      FnSub(buf.toString, None)
    }
  }

}
