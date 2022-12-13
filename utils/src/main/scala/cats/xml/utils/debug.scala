package cats.xml.utils

import cats.Show

case class Debug(
  xmlPrinterPrintTypesName: Boolean,
  doNotOverrideXmlToString: Boolean
)
private[xml] object Debug extends DebugSyntax {

  private var _debug: Option[Debug] = None

  def enable(
    xmlPrinterPrintTypesName: Boolean = false,
    doNotOverrideXmlToString: Boolean = false
  ): Unit = _debug = Some(
    Debug(
      xmlPrinterPrintTypesName,
      doNotOverrideXmlToString
    )
  )

  def disable(): Unit = _debug = None

  def debug: Option[Debug] = _debug

  def enabled: Boolean = debug.isDefined

  def enabledAnd(p: Debug => Boolean): Boolean = debug.exists(p)

  def disabled: Boolean = debug.isEmpty

  def ifEnabledAnd[T](p: Debug => Boolean)(ifTrue: => T, ifFalse: => T): T =
    debug.filter(p).fold(ifFalse)(_ => ifTrue)

  def ifEnabled[T](f: Debug => T, ifDisabled: => T): T =
    debug.fold(ifDisabled)(f)

  def ifEnabled(f: Debug => Unit): Unit =
    debug.fold(())(f)

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
