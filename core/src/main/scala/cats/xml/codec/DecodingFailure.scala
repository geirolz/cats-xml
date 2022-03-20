package cats.xml.codec

import cats.xml.Xml
import cats.xml.cursor.CursorResult
import cats.Show

trait DecodingResult

case class DecodingFailure(reason: DecodingFailureReason)
object DecodingFailure {

  def error(ex: Throwable): DecodingFailure =
    DecodingFailure(DecodingFailureReason.Error(ex))

  def noTextAvailable(subject: Xml): DecodingFailure =
    DecodingFailure(DecodingFailureReason.NoTextAvailable(subject))

  def cursorFailure(failed: CursorResult.Failed): DecodingFailure =
    DecodingFailure(DecodingFailureReason.CursorFailure(failed))

  def custom(message: String): DecodingFailure =
    DecodingFailure(DecodingFailureReason.Custom(message))

  def coproductNomatch(actual: Any, coproductValues: Seq[Any]): DecodingFailure =
    DecodingFailure(DecodingFailureReason.CoproductUnmatch(actual, coproductValues))
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

  implicit val showDecodingFailureReason: Show[DecodingFailureReason] = {
    case Error(ex)                => s"Exception: ${ex.getMessage}"
    case NoTextAvailable(subject) => s"No text available inside $subject"
    case CursorFailure(failed)    => failed.toString
    case Custom(message)          => message
    case CoproductUnmatch(actual, vals) =>
      s"Value '$actual' not in [${vals.mkString(", ")}]"
  }
}
