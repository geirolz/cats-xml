package cats.xml.cursor

import cats.{Eq, Show}
import cats.data.NonEmptyList
import cats.xml.codec.DecoderFailure
import cats.xml.cursor.CursorFailure.CursorFailureException
import cats.xml.utils.UnderlyingThrowableWeakEq
import cats.xml.XmlNode

/** A coproduct ADT to represent the `Cursor` possible failures.
  */
sealed trait CursorFailure {

  def asException: CursorFailureException = CursorFailureException(this)

  final val isMissing: Boolean = this match {
    case _: CursorFailure.Missing => true
    case _                        => false
  }

  override def toString: String = Show[CursorFailure].show(this)
}
object CursorFailure {

  final case class InvalidTargetType(required: Class[?], actual: Class[?]) extends CursorFailure

  // decode
  final case class DecoderFailed(path: String, failure: DecoderFailure) extends CursorFailure

  // validation
  final case class ValidationsFailed(path: String, errors: NonEmptyList[String]) extends FailedNode

  // node
  sealed trait Missing extends CursorFailure {
    val path: String
  }
  sealed trait FailedNode extends CursorFailure
  final case class MissingNode(path: String, nodeName: String) extends FailedNode with Missing
  final case class MissingNodeAtIndex(path: String, index: Int) extends FailedNode with Missing
  final case class MissingNodeFind(path: String, predicate: XmlNode => Boolean)
      extends FailedNode
      with Missing
  final case class MissingNodeHead(path: String) extends FailedNode with Missing
  final case class MissingNodeLast(path: String) extends FailedNode with Missing
  final case class MissingText(path: String) extends FailedNode with Missing

  sealed trait FailedAttribute extends CursorFailure
  final case class MissingAttrByKey(path: String, key: String) extends FailedAttribute with Missing
  final case class MissingAttrAtIndex(path: String, index: Long)
      extends FailedAttribute
      with Missing
  final case class MissingAttrHead(path: String) extends FailedAttribute with Missing
  final case class MissingAttrLast(path: String) extends FailedAttribute with Missing
  final case class LeftBoundLimitAttr(path: String, lastKey: String)
      extends FailedAttribute
      with Missing
  final case class RightBoundLimitAttr(path: String, lastKey: String)
      extends FailedAttribute
      with Missing
  final case class Custom(message: String) extends CursorFailure
  final case class Error(error: Throwable) extends CursorFailure with UnderlyingThrowableWeakEq

  final case class CursorFailureException(failure: CursorFailure)
      extends RuntimeException(s"Cursor failure: $failure")

  implicit val eqCursorFailureReason: Eq[CursorFailure] =
    Eq.fromUniversalEquals

  implicit def showCursorFailureReason: Show[CursorFailure] = failure => {

    def pathAt(path: String): String = path match {
      case "" => ""
      case p  => s" at '$p'"
    }

    failure match {
      case InvalidTargetType(required, actual) =>
        s"Wrong XML element type, '$required' required but got '$actual'"
      case MissingAttrByKey(path, key)     => s"Missing attribute '$key'${pathAt(path)}"
      case MissingAttrAtIndex(path, index) => s"Missing attribute at index '$index'${pathAt(path)}"
      case MissingAttrHead(path)           => s"Head attribute on empty list${pathAt(path)}"
      case MissingAttrLast(path)           => s"Last attribute on empty list${pathAt(path)}"
      case MissingNode(path, nodeName)     => s"Missing node '$nodeName'${pathAt(path)}"
      case MissingNodeAtIndex(path, index) => s"Missing node at index '$index'${pathAt(path)}"
      case MissingNodeFind(path, _)        => s"Missing node find${pathAt(path)}"
      case MissingNodeHead(path)           => s"Missing head node${pathAt(path)}"
      case MissingNodeLast(path)           => s"Missing last node${pathAt(path)}"
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
