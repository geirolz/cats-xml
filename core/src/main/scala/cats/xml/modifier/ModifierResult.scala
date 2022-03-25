package cats.xml.modifier

import cats.xml.cursor.CursorResult.Failed
import cats.xml.modifier.ModifierResult.{Modified, ModifierFailed}
import cats.{MonadError, Show, StackSafeMonad}

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

  def handleError[U >: T](f: ModifierFailed => U): ModifierResult[U] =
    handleErrorWith(failed => ModifierResult.Modified(f(failed)))

  def handleErrorWith[A](f: ModifierFailed => ModifierResult[A]): ModifierResult[A] =
    this match {
      case modified: ModifierResult.Modified[?] => modified.asInstanceOf[Modified[A]]
      case failed: ModifierFailed               => f(failed)
    }

  def fold[TT >: T, U](ifModified: TT => U, ifFailed: ModifierFailed => U): U =
    this match {
      case ModifierResult.Modified(value) => ifModified(value)
      case failed: ModifierFailed         => ifFailed(failed)
    }

  override def toString: String = Show[ModifierResult[T]].show(this)
}
object ModifierResult extends ModifierResultInstances {

  // success
  case class Modified[T](value: T) extends ModifierResult[T]

  // failed
  sealed trait ModifierFailed extends ModifierResult[Nothing]
  case class InvalidData[D](message: String, data: D) extends ModifierFailed
  case class CursorFailed(cursorFailed: Failed) extends ModifierFailed
  case class Custom(message: String) extends ModifierFailed
}

sealed trait ModifierResultInstances {

  implicit def monadForModifierResult: MonadError[ModifierResult, ModifierFailed] =
    new MonadError[ModifierResult, ModifierFailed] with StackSafeMonad[ModifierResult] {

      override def pure[A](x: A): ModifierResult[A] = ModifierResult.Modified(x)

      override def raiseError[A](e: ModifierFailed): ModifierResult[A] = e

      override def flatMap[A, B](fa: ModifierResult[A])(
        f: A => ModifierResult[B]
      ): ModifierResult[B] = fa.flatMap(f)

      override def handleErrorWith[A](fa: ModifierResult[A])(
        f: ModifierFailed => ModifierResult[A]
      ): ModifierResult[A] =
        fa.handleErrorWith(f)
    }

  implicit def showModifierResult[T](implicit
    showT: Show[T] = Show.fromToString[T]
  ): Show[ModifierResult[T]] = {
    case ModifierResult.Modified(value)            => s"Modified(${showT.show(value)})"
    case ModifierResult.InvalidData(message, data) => s"Invalid data $data. $message"
    case ModifierResult.CursorFailed(cursorFailed) => cursorFailed.toString
    case ModifierResult.Custom(message)            => s"Modifier failed: $message"
  }
}
