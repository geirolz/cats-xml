package cats.xml.modifier

import cats.Show
import cats.data.NonEmptyList
import cats.xml.codec.DecoderFailure
import cats.xml.cursor.CursorFailure
import cats.xml.modifier.ModifierFailure.ModifierFailureException
import cats.xml.utils.UnderlyingThrowableWeakEq

/** A coproduct ADT to represent the `Modifier` possible failures.
  */
sealed trait ModifierFailure {

  def asException: ModifierFailureException = ModifierFailureException(this)

  override def toString: String = Show[ModifierFailure].show(this)
}
object ModifierFailure {

  final case class InvalidData[D](message: String, data: D) extends ModifierFailure
  final case class CursorFailed(cursorFailures: NonEmptyList[CursorFailure]) extends ModifierFailure
  final case class DecoderFailed(decoderFailures: NonEmptyList[DecoderFailure])
      extends ModifierFailure
  final case class Custom(message: String) extends ModifierFailure
  final case class Error(error: Throwable) extends ModifierFailure with UnderlyingThrowableWeakEq

  final case class ModifierFailureException(failure: ModifierFailure)
      extends RuntimeException(s"Modifier failure: $failure")

  implicit val showModifierFailureReason: Show[ModifierFailure] = {
    case InvalidData(message, data)     => s"Invalid data $data. $message"
    case CursorFailed(cursorFailures)   => cursorFailures.toString
    case DecoderFailed(decoderFailures) => decoderFailures.toString
    case Custom(message)                => message
    case Error(e)                       => e.getMessage
  }
}
