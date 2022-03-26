package cats.xml.modifier

import cats.{MonadError, MonadThrow, Show, StackSafeMonad}
import cats.data.NonEmptyList
import cats.xml.cursor.CursorResult
import cats.xml.modifier.ModifierResult.*

sealed trait ModifierResult[+T] {

  val isModified: Boolean =
    fold[T, Boolean](
      ifModified = _ => true,
      ifFailed   = _ => false
    )

  val isFailed: Boolean = !isModified

  def map[U](f: T => U): ModifierResult[U] =
    flatMap(f.andThen(ModifierResult.Modified(_)))

  def flatMap[U](f: T => ModifierResult[U]): ModifierResult[U] =
    fold(f, identity)

  def fold[TT >: T, U](ifModified: TT => U, ifFailed: ModifierResult.Failed => U): U =
    this match {
      case ModifierResult.Modified(value) => ifModified(value)
      case failed: Failed                 => ifFailed(failed)
    }

  def handleError[U >: T](f: ModifierResult.Failed => U): ModifierResult[U] =
    handleErrorWith(failed => ModifierResult.Modified(f(failed)))

  def handleErrorWith[A](f: ModifierResult.Failed => ModifierResult[A]): ModifierResult[A] =
    this match {
      case modified: ModifierResult.Modified[?] => modified.asInstanceOf[Modified[A]]
      case failed: Failed                       => f(failed)
    }

  def toOption[U >: T]: Option[U] =
    this match {
      case Modified(value) => Some(value)
      case _               => None
    }

  def attempt[U >: T]: ModifierResult[Either[ModifierResult.Failed, U]] =
    this match {
      case ModifierResult.Modified(value) => Modified(Right(value))
      case failed: ModifierResult.Failed  => Modified(Left(failed))
    }

  def attemptOption[U >: T]: ModifierResult[Option[U]] =
    this match {
      case Modified(value) => Modified(Some(value))
      case _               => Modified(None)
    }

  override def toString: String =
    Show[ModifierResult[T]].show(this)
}
object ModifierResult extends ModifierResultInstances with ModifierResultSyntax {

  // success
  case class Modified[T](value: T) extends ModifierResult[T]

  // failed
  sealed trait Failed extends ModifierResult[Nothing] {
    def asException: ModifierFailureException = ModifierFailureException(NonEmptyList.one(this))
  }
  case class InvalidData[D](message: String, data: D) extends Failed
  case class CursorFailed(cursorFailed: CursorResult.Failed) extends Failed

  // exception
  case class CustomError(message: String) extends Failed
  case class ExceptionOccurred(exception: Throwable) extends Failed
  case class ModifierFailureException(failures: NonEmptyList[Failed])
      extends RuntimeException(s"Modifier failures: ${failures.toList.mkString("\n")}")

}
private[xml] trait ModifierResultSyntax {
  implicit class NelModifierResultFailedOps(ls: NonEmptyList[ModifierResult.Failed]) {
    def asException: ModifierFailureException = ModifierFailureException(ls)
  }
}
sealed trait ModifierResultInstances {

  implicit def monadThrowCursorResult(implicit
    me: MonadError[ModifierResult, ModifierResult.Failed]
  ): MonadThrow[ModifierResult] =
    new MonadThrow[ModifierResult] with StackSafeMonad[ModifierResult] {

      override def pure[A](x: A): ModifierResult[A] =
        me.pure(x)

      override def flatMap[A, B](fa: ModifierResult[A])(
        f: A => ModifierResult[B]
      ): ModifierResult[B] =
        me.flatMap(fa)(f)

      override def raiseError[A](e: Throwable): ModifierResult[A] =
        ModifierResult.ExceptionOccurred(e)

      override def handleErrorWith[A](fa: ModifierResult[A])(
        f: Throwable => ModifierResult[A]
      ): ModifierResult[A] =
        fa.handleErrorWith(error => f(error.asException))
    }

  implicit val monadForModifierResult: MonadError[ModifierResult, ModifierResult.Failed] =
    new MonadError[ModifierResult, ModifierResult.Failed] with StackSafeMonad[ModifierResult] {

      override def pure[A](x: A): ModifierResult[A] = ModifierResult.Modified(x)

      override def raiseError[A](e: ModifierResult.Failed): ModifierResult[A] = e

      override def flatMap[A, B](fa: ModifierResult[A])(
        f: A => ModifierResult[B]
      ): ModifierResult[B] = fa.flatMap(f)

      override def handleErrorWith[A](fa: ModifierResult[A])(
        f: ModifierResult.Failed => ModifierResult[A]
      ): ModifierResult[A] =
        fa.handleErrorWith(f)
    }

  implicit def showModifierResult[T](implicit
    showT: Show[T] = Show.fromToString[T]
  ): Show[ModifierResult[T]] = {
    case Modified(value)            => s"Modified(${showT.show(value)})"
    case InvalidData(message, data) => s"Invalid data $data. $message"
    case CursorFailed(cursorFailed) => cursorFailed.toString
    case CustomError(message)       => s"Modifier failed: $message"
    case ExceptionOccurred(e)       => s"Modifier failed: ${e.getMessage}"
  }
}
