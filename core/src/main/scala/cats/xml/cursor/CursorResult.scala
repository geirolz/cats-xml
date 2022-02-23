package cats.xml.cursor

import cats.xml.codec.Decoder
import cats.xml.cursor.CursorResult.*
import cats.Show

sealed trait CursorResult[+T] {

  def map[U](f: T => U): CursorResult[U] =
    flatMap(f.andThen(Focused(_)))

  def flatMap[U](f: T => CursorResult[U]): CursorResult[U] =
    fold(f)(identity)

  def fold[U](ifFocused: T => U)(ifFailed: Failed => U): U = this match {
    case Focused(target) => ifFocused(target)
    case failed: Failed  => ifFailed(failed)
  }

  def recover[U >: T](f: Failed => U): CursorResult[U] =
    recoverWith(failed => Focused(f(failed)))

  def recoverWith[U >: T](f: Failed => CursorResult[U]): CursorResult[U] =
    this match {
      case Focused(target) => Focused[U](target)
      case failed: Failed  => f(failed)
    }

  /** Return an `Option` which contains a value only if the result is `Focused`
    */
  def toOption[U >: T]: Option[U] =
    this match {
      case Focused(value) => Some(value)
      case _              => None
    }

  def attempt[U >: T]: CursorResult[Either[Failed, U]] =
    this match {
      case Focused(value) => Focused(Right(value))
      case failed: Failed => Focused(Left(failed))
    }

  /** Lift the result to `Option[T]`, any error is mapped as `None`
    */
  def attemptOption[U >: T]: CursorResult[Option[U]] =
    this match {
      case Focused(value) => Focused(Some(value))
      case _              => Focused(None)
    }

  override def toString: String =
    Show[CursorResult[T]].show(this)
}

object CursorResult extends CursorResultInstances {

  case class Focused[T](value: T) extends CursorResult[T]
  trait Failed extends CursorResult[Nothing] {
    val path: String
    def asException: CursorFailureException = CursorFailureException(this)
  }
  sealed trait Missing extends Failed {
    val path: String
  }

  // decode
  case class CursorDecodingFailure(path: String, error: Decoder.InvalidResult) extends Failed

  // node
  trait FailedNode extends Failed
  case class MissingNode(nodeName: String, path: String) extends FailedNode with Missing

  // text
  case class MissingText(path: String) extends FailedNode with Missing

  // attrs
  sealed trait FailedAttribute extends Failed
  case class MissingAttrByKey(path: String, key: String) extends FailedAttribute with Missing
  case class MissingAttrAtIndex(path: String, index: Long) extends FailedAttribute with Missing
  case class MissingAttrHead(path: String) extends FailedAttribute with Missing
  case class MissingAttrLast(path: String) extends FailedAttribute with Missing
  case class LeftBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing
  case class RightBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing

  // ops
  case class CursorFailureException(failed: Failed) extends RuntimeException(failed.toString)
  def fromOption[T](opt: Option[T])(ifEmpty: => CursorResult[T]): CursorResult[T] =
    opt.fold(ifEmpty)(Focused(_))
}
private[xml] sealed trait CursorResultInstances {

  import cats.implicits.*

  implicit def showInstanceForCursorResult[T](implicit
    showT: Show[T] = Show.fromToString[T]
  ): Show[CursorResult[T]] = {
    case Focused(value)                     => Show[T].show(value)
    case MissingAttrByKey(path, key)        => s"Missing attribute '$key' at '$path'"
    case MissingAttrAtIndex(path, index)    => s"Missing attribute at index '$index' at '$path'"
    case MissingAttrHead(path)              => s"Head attribute on empty list at '$path'"
    case MissingAttrLast(path)              => s"Last attribute on empty list at '$path'"
    case LeftBoundLimitAttr(path, lastKey)  => s"Reached left bound limit attribute at '$path'"
    case RightBoundLimitAttr(path, lastKey) => s"Reached right bound limit attribute at '$path'"
    case MissingNode(nodeName, path)        => s"Missing node '$nodeName' at '$path'"
    case MissingText(path)                  => s"Missing text at '$path'"
    case CursorDecodingFailure(path, error) =>
      s"Unable to decode value at '$path'. ${error.e.map(_.reason).mkString_(", ")}"

    case unmappedError => unmappedError.toString
  }
}
