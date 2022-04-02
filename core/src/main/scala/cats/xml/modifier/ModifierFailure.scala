package cats.xml.modifier

import cats.Show
import cats.xml.cursor.CursorFailure
import cats.xml.modifier.ModifierFailure.ModifierFailureException
import cats.xml.utils.ThrowableKeeper

/** A coproduct ADT to represent the `Modifier` possible failures.
  */
sealed trait ModifierFailure {

  def asException: ModifierFailureException = ModifierFailureException(this)

  override def toString: String = Show[ModifierFailure].show(this)
}
object ModifierFailure {

  case class InvalidData[D](message: String, data: D) extends ModifierFailure
  case class CursorFailed(cursorFailure: CursorFailure) extends ModifierFailure
  case class Custom(message: String) extends ModifierFailure
  case class Error(ex: Throwable) extends ModifierFailure with ThrowableKeeper

  case class ModifierFailureException(failure: ModifierFailure)
      extends RuntimeException(s"Modifier failure: $failure")

  implicit val showModifierFailureReason: Show[ModifierFailure] = {
    case InvalidData(message, data) => s"Invalid data $data. $message"
    case CursorFailed(cursorFailed) => cursorFailed.toString
    case Custom(message)            => message
    case Error(e)                   => e.getMessage
  }
}
