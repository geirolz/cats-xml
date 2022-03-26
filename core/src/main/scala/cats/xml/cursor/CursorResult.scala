package cats.xml.cursor

import cats.{MonadError, MonadThrow, Show, StackSafeMonad}
import cats.data.NonEmptyList
import cats.xml.codec.Decoder
import cats.xml.cursor.CursorResult.*

sealed trait CursorResult[+T] {

  val isFocused: Boolean =
    fold[T, Boolean](
      ifFocused = _ => true,
      ifFailed  = _ => false
    )

  val isFailed: Boolean = !isFocused

  def map[U](f: T => U): CursorResult[U] =
    flatMap(f.andThen(CursorResult.Focused(_)))

  def flatMap[U](f: T => CursorResult[U]): CursorResult[U] =
    fold(f, identity)

  def fold[TT >: T, U](ifFocused: TT => U, ifFailed: CursorResult.Failed => U): U =
    this match {
      case CursorResult.Focused(target) => ifFocused(target)
      case failed: CursorResult.Failed  => ifFailed(failed)
    }

  def handleError[U >: T](f: CursorResult.Failed => U): CursorResult[U] =
    handleErrorWith(failed => CursorResult.Focused(f(failed)))

  def handleErrorWith[U >: T](f: CursorResult.Failed => CursorResult[U]): CursorResult[U] =
    this match {
      case Focused(target) => CursorResult.Focused[U](target)
      case failed: Failed  => f(failed)
    }

  def toOption[U >: T]: Option[U] =
    this match {
      case Focused(value) => Some(value)
      case _              => None
    }

  def attempt[U >: T]: CursorResult[Either[CursorResult.Failed, U]] =
    this match {
      case Focused(value) => Focused(Right(value))
      case failed: Failed => Focused(Left(failed))
    }

  def attemptOption[U >: T]: CursorResult[Option[U]] =
    attempt[U].map(_.toOption)

  override def toString: String =
    Show[CursorResult[T]].show(this)
}

object CursorResult extends CursorResultInstances with CursorResultSyntax {

  case class Focused[T](value: T) extends CursorResult[T]
  sealed trait Failed extends CursorResult[Nothing] {
    def asException: CursorFailureException = CursorFailureException(NonEmptyList.one(this))
  }
  sealed trait Missing extends Failed {
    val path: String
  }

  // decode
  case class CursorDecodingFailure(path: String, error: Decoder.InvalidResult) extends Failed

  // node
  sealed trait FailedNode extends Failed
  case class MissingNode(nodeName: String, path: String) extends FailedNode with Missing
  case class MissingText(path: String) extends FailedNode with Missing

  sealed trait FailedAttribute extends Failed
  case class MissingAttrByKey(path: String, key: String) extends FailedAttribute with Missing
  case class MissingAttrAtIndex(path: String, index: Long) extends FailedAttribute with Missing
  case class MissingAttrHead(path: String) extends FailedAttribute with Missing
  case class MissingAttrLast(path: String) extends FailedAttribute with Missing
  case class LeftBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing
  case class RightBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing

  // exception
  case class CustomError(message: String) extends Failed
  case class ExceptionOccurred(exception: Throwable) extends Failed
  case class CursorFailureException(failures: NonEmptyList[Failed])
      extends RuntimeException(s"Cursor failures: ${failures.toList.mkString("\n")}")

  def fromOption[T](opt: Option[T])(ifEmpty: => CursorResult[T]): CursorResult[T] =
    opt.fold(ifEmpty)(Focused(_))
}
private[xml] trait CursorResultSyntax {
  implicit class NelCursorResultFailedOps(ls: NonEmptyList[CursorResult.Failed]) {
    def asException: CursorFailureException = CursorFailureException(ls)
  }
}
private[xml] sealed trait CursorResultInstances {

  import cats.implicits.*

  implicit def monadThrowCursorResult(implicit
    me: MonadError[CursorResult, CursorResult.Failed]
  ): MonadThrow[CursorResult] =
    new MonadThrow[CursorResult] with StackSafeMonad[CursorResult] {

      override def pure[A](x: A): CursorResult[A] =
        me.pure(x)

      override def flatMap[A, B](fa: CursorResult[A])(f: A => CursorResult[B]): CursorResult[B] =
        me.flatMap(fa)(f)

      override def raiseError[A](e: Throwable): CursorResult[A] =
        CursorResult.ExceptionOccurred(e)

      override def handleErrorWith[A](fa: CursorResult[A])(
        f: Throwable => CursorResult[A]
      ): CursorResult[A] =
        fa.handleErrorWith(error => f(error.asException))
    }

  implicit val monadErrorCursorResult: MonadError[CursorResult, CursorResult.Failed] =
    new MonadError[CursorResult, CursorResult.Failed] with StackSafeMonad[CursorResult] {

      override def pure[A](x: A): CursorResult[A] =
        CursorResult.Focused(x)

      override def raiseError[A](e: Failed): CursorResult[A] = e

      override def flatMap[A, B](fa: CursorResult[A])(f: A => CursorResult[B]): CursorResult[B] =
        fa.flatMap(f)

      override def handleErrorWith[A](fa: CursorResult[A])(
        f: Failed => CursorResult[A]
      ): CursorResult[A] =
        fa.handleErrorWith(f)

    }

  implicit def showCursorResult[T](implicit
    showT: Show[T] = Show.fromToString[T]
  ): Show[CursorResult[T]] = {
    case Focused(value)                  => s"Focused(${showT.show(value)})"
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
    case CursorDecodingFailure(path, error) =>
      s"Unable to decode value at '$path'. ${error.e.map(_.reason).mkString_(", ")}"

    case CustomError(message) => s"Cursor failed: $message"
    case ExceptionOccurred(e) => s"Cursor failed: ${e.getMessage}"
  }
}
