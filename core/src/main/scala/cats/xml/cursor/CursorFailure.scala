package cats.xml.cursor

import cats.{Eq, Show}
import cats.data.NonEmptyList
import cats.xml.codec.DecoderFailure
import cats.xml.cursor.CursorFailure.CursorFailureException

/** A coproduct ADT to represent the `Cursor` possible failures.
  */
sealed trait CursorFailure {

  def asException: CursorFailureException = CursorFailureException(this)

  override def toString: String = Show[CursorFailure].show(this)
}
object CursorFailure {

  // decode
  case class DecoderFailed(
    path: String,
    failures: NonEmptyList[DecoderFailure]
  ) extends CursorFailure

  // node
  sealed trait Missing extends CursorFailure {
    val path: String
  }
  sealed trait FailedNode extends CursorFailure
  case class MissingNode(nodeName: String, path: String) extends FailedNode with Missing
  case class MissingText(path: String) extends FailedNode with Missing

  sealed trait FailedAttribute extends CursorFailure
  case class MissingAttrByKey(path: String, key: String) extends FailedAttribute with Missing
  case class MissingAttrAtIndex(path: String, index: Long) extends FailedAttribute with Missing
  case class MissingAttrHead(path: String) extends FailedAttribute with Missing
  case class MissingAttrLast(path: String) extends FailedAttribute with Missing
  case class LeftBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing
  case class RightBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing
  case class Custom(message: String) extends CursorFailure
  case class Error(ex: Throwable) extends CursorFailure

  case class CursorFailureException(failure: CursorFailure)
      extends RuntimeException(s"Cursor failure: $failure")

  implicit val eqCursorFailureReason: Eq[CursorFailure] =
    Eq.fromUniversalEquals

  implicit def showCursorFailureReason: Show[CursorFailure] = {
    case MissingAttrByKey(path, key)     => s"Missing attribute '$key' at '$path'"
    case MissingAttrAtIndex(path, index) => s"Missing attribute at index '$index' at '$path'"
    case MissingAttrHead(path)           => s"Head attribute on empty list at '$path'"
    case MissingAttrLast(path)           => s"Last attribute on empty list at '$path'"
    case MissingNode(nodeName, path)     => s"Missing node '$nodeName' at '$path'"
    case MissingText(path)               => s"Missing text at '$path'"
    case LeftBoundLimitAttr(path, lastKey) =>
      s"Reached left bound limit attribute at '$path', last valid key '$lastKey'"
    case RightBoundLimitAttr(path, lastKey) =>
      s"Reached right bound limit attribute at '$path', last valid key '$lastKey'"
    case DecoderFailed(path, failures) =>
      s"Unable to decode value at '$path'. ${failures.toList.mkString("\n")}"
    case Custom(message) => message
    case Error(e)        => e.getMessage
  }
}
