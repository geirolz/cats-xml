package cats.xml.codec

import cats.{Eq, Show}
import cats.data.{NonEmptyList, Validated}
import cats.xml.Xml
import cats.xml.codec.DecoderFailure.DecoderFailureException
import cats.xml.cursor.CursorFailure
import cats.xml.utils.UnderlyingThrowableWeakEq

import scala.reflect.ClassTag
import scala.util.Try

sealed trait DecoderFailure {

  def asException: DecoderFailureException = DecoderFailureException(NonEmptyList.one(this))

  override def toString: String = Show[DecoderFailure].show(this)
}
object DecoderFailure extends DecoderFailureSyntax {
  case class NoTextAvailable(subject: Xml) extends DecoderFailure
  case class CursorFailed(failure: CursorFailure) extends DecoderFailure
  case class CoproductNoMatch[+T](actual: Any, coproductValues: Seq[T]) extends DecoderFailure
  case class Error(error: Throwable) extends DecoderFailure with UnderlyingThrowableWeakEq
  case class UnableToDecodeType[T: ClassTag](value: Any) extends DecoderFailure {
    def intoSimpleClassName: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName
  }
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
    case e @ UnableToDecodeType(value)  => s"Unable to decode $value to ${e.intoSimpleClassName}"
    case Custom(message)                => message
  }
}
trait DecoderFailureSyntax {

  import cats.syntax.validated.*

  implicit class ClassDecoderFailureNelOps(nel: NonEmptyList[DecoderFailure]) {
    def asException: DecoderFailureException = DecoderFailureException(nel)
  }

  implicit class TryToValidatedNel[A](tryValue: Try[A]) {

    def toValidatedNel[E](
      onFailure: Throwable => E
    ): Validated[NonEmptyList[E], A] =
      tryValue.fold(onFailure(_).invalidNel, _.validNel)
  }
}
