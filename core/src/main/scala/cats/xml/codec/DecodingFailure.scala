package cats.xml.codec

import cats.{Eq, Show}
import cats.xml.Xml
import cats.xml.cursor.CursorResult

case class DecodingFailure(reason: DecodingFailureReason)
object DecodingFailure extends DecodingFailureInstances {

  def error(ex: Throwable): DecodingFailure =
    DecodingFailure(DecodingFailureReason.Error(ex))

  def noTextAvailable(subject: Xml): DecodingFailure =
    DecodingFailure(DecodingFailureReason.NoTextAvailable(subject))

  def cursorFailure(failed: CursorResult.Failed): DecodingFailure =
    DecodingFailure(DecodingFailureReason.CursorFailure(failed))

  def custom(message: String): DecodingFailure =
    DecodingFailure(DecodingFailureReason.Custom(message))

  def coproductNoMatch(actual: Any, coproductValues: Seq[Any]): DecodingFailure =
    DecodingFailure(DecodingFailureReason.CoproductUnmatch(actual, coproductValues))
}

sealed trait DecodingFailureInstances {

  implicit def eqDecodingFailure(implicit eqR: Eq[DecodingFailureReason]): Eq[DecodingFailure] =
    (x: DecodingFailure, y: DecodingFailure) => eqR.eqv(x.reason, y.reason)
}

sealed trait DecodingFailureReason {
  override def toString: String = Show[DecodingFailureReason].show(this)
}
object DecodingFailureReason {
  case class Error(ex: Throwable) extends DecodingFailureReason
  case class NoTextAvailable(subject: Xml) extends DecodingFailureReason
  case class CursorFailure(failed: CursorResult.Failed) extends DecodingFailureReason
  case class CoproductUnmatch[T](actual: Any, coproductValues: Seq[? <: T])
      extends DecodingFailureReason
  case class Custom(message: String) extends DecodingFailureReason

  implicit val eqDecodingFailureReason: Eq[DecodingFailureReason] =
    Eq.fromUniversalEquals

  implicit val showDecodingFailureReason: Show[DecodingFailureReason] = {
    case Error(ex)                => s"Exception: ${ex.getMessage}"
    case NoTextAvailable(subject) => s"No text available inside $subject"
    case CursorFailure(failed)    => failed.toString
    case Custom(message)          => message
    case CoproductUnmatch(actual, vals) =>
      s"Value '$actual' not in [${vals.mkString(", ")}]"
  }
}
