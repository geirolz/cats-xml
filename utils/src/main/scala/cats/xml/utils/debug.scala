package cats.xml.utils

import cats.Show

private[xml] object Debug extends DebugSyntax {

  def debug[T](any: T)(implicit s: Show[T] = Show.fromToString[T]): T = {
    Console.println(s"""
         |## DEBUG ##
         |Where: $currentLocationInfo
         |Msg: ${Show[T].show(any)}""".stripMargin)
    any
  }

  def currentLocationInfo: String =
    new RuntimeException().getStackTrace.toList
      .drop(3)
      .headOption
      .map(_.toString)
      .getOrElse("*UNKNOWN*")
}
private[xml] trait DebugSyntax {

  implicit class DebugAnyOps[T](any: T) {
    def debug(implicit s: Show[T] = Show.fromToString[T]): T = Debug.debug(any)
  }
}
