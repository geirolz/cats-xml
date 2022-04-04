package cats.xml.cursor

import cats.{Eq, Show}
import cats.data.NonEmptyList
import cats.xml.codec.DecoderFailure
import cats.xml.cursor.CursorFailure.CursorFailureException
import cats.xml.utils.ErrorKeeper

/** A coproduct ADT to represent the `Cursor` possible failures.
  */
sealed trait CursorFailure {

  def asException: CursorFailureException = CursorFailureException(this)

  override def toString: String = Show[CursorFailure].show(this)
}
object CursorFailure {

  // decode
  case class DecoderFailed(path: String, failure: DecoderFailure) extends CursorFailure

  // validation
  case class ValidationsFailed(path: String, errors: NonEmptyList[String]) extends FailedNode

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
  case class Error(error: Throwable) extends CursorFailure with ErrorKeeper

  case class CursorFailureException(failure: CursorFailure)
      extends RuntimeException(s"Cursor failure: $failure")

  implicit val eqCursorFailureReason: Eq[CursorFailure] =
    Eq.fromUniversalEquals

  implicit def showCursorFailureReason: Show[CursorFailure] = failure => {

    def pathAt(path: String): String = path match {
      case "" => ""
      case p  => s" at '$p'"
    }

    failure match {
      case MissingAttrByKey(path, key)     => s"Missing attribute '$key'${pathAt(path)}"
      case MissingAttrAtIndex(path, index) => s"Missing attribute at index '$index'${pathAt(path)}"
      case MissingAttrHead(path)           => s"Head attribute on empty list${pathAt(path)}"
      case MissingAttrLast(path)           => s"Last attribute on empty list${pathAt(path)}"
      case MissingNode(nodeName, path)     => s"Missing node '$nodeName'${pathAt(path)}"
      case MissingText(path)               => s"Missing text${pathAt(path)}'"
      case ValidationsFailed(path, eNel) =>
        s"Validations (${eNel.size}) failed${pathAt(path)}. ${eNel.toList.mkString("\n- ", "\n- ", "")}"
      case LeftBoundLimitAttr(path, lastKey) =>
        s"Reached left bound limit attribute${pathAt(path)}, last valid key '$lastKey'"
      case RightBoundLimitAttr(path, lastKey) =>
        s"Reached right bound limit attribute${pathAt(path)}, last valid key '$lastKey'"
      case DecoderFailed(path, failure) =>
        s"Unable to decode value${pathAt(path)}. $failure"
      case Custom(message) => message
      case Error(e)        => e.getMessage
    }
  }
}
