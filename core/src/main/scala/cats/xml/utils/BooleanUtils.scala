package cats.xml.utils

object BooleanUtils {
  implicit class BooleanOps(c: Boolean) {

    def ?[T](ifTrue: => T): TernaryOpPartialApply[T] =
      new TernaryOpPartialApply[T](c, ifTrue)

    class TernaryOpPartialApply[T](c: Boolean, ifTrue: => T) {
      def |(ifFalse: => T): T = if (c) ifTrue else ifFalse
    }
  }
}
