package cats.xml.codec

import cats.{Eq, Show}
import cats.data.NonEmptyList
import cats.xml.Xml
import cats.xml.codec.DecoderFailure.DecoderFailureException
import cats.xml.cursor.CursorFailure

sealed trait DecoderFailure {

  def asException: DecoderFailureException = DecoderFailureException(NonEmptyList.one(this))

  override def toString: String = Show[DecoderFailure].show(this)
}
object DecoderFailure extends DecoderFailureSyntax {
  case class NoTextAvailable(subject: Xml) extends DecoderFailure
  case class CursorFailed(failure: CursorFailure) extends DecoderFailure
  case class CoproductNoMatch[+T](actual: Any, coproductValues: Seq[T]) extends DecoderFailure
  case class Error(ex: Throwable) extends DecoderFailure
  case class Custom(message: String) extends DecoderFailure

  case class DecoderFailureException(failures: NonEmptyList[DecoderFailure])
      extends RuntimeException(s"Decoder failure: ${failures.toList.mkString("\n")}")

  implicit val eqDecodingFailureReason: Eq[DecoderFailure] =
    Eq.fromUniversalEquals

  implicit val showDecodingFailureReason: Show[DecoderFailure] = {
    case NoTextAvailable(subject)       => s"No text available inside $subject"
    case CursorFailed(failed)           => failed.toString
    case CoproductNoMatch(actual, vals) => s"Value '$actual' not in [${vals.mkString(", ")}]"
    case Error(ex)                      => s"Exception: ${ex.getMessage}"
    case Custom(message)                => message
  }
}
trait DecoderFailureSyntax {
  implicit class classDecoderFailureNelOps(nel: NonEmptyList[DecoderFailure]) {
    def asException: DecoderFailureException = DecoderFailureException(nel)
  }
}
