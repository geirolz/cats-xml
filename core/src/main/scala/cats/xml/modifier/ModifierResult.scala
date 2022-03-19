package cats.xml.modifier

import cats.xml.cursor.CursorResult.Failed
import cats.xml.modifier.ModifierResult.{Modified, ModifierFailed}
import cats.{MonadError, StackSafeMonad}

sealed trait ModifierResult[+T] {

  val isModified: Boolean =
    fold[T, Boolean](ifModified = _ => true, ifFailed = _ => false)

  val isFailed: Boolean = !isModified

  def map[U](f: T => U): ModifierResult[U] =
    flatMap(f.andThen(ModifierResult.Modified(_)))

  def flatMap[U](f: T => ModifierResult[U]): ModifierResult[U] =
    fold(f, identity)

  def fold[TT >: T, U](ifModified: TT => U, ifFailed: ModifierFailed => U): U =
    this match {
      case ModifierResult.Modified(value) => ifModified(value)
      case failed: ModifierFailed         => ifFailed(failed)
    }
}
object ModifierResult extends ModifierResultInstances {

  def pure[T](value: T): ModifierResult[T] = Modified(value)

  // success
  case class Modified[T](value: T) extends ModifierResult[T]

  // failed
  sealed trait ModifierFailed extends ModifierResult[Nothing]
  case class InvalidData[D](message: String, data: D) extends ModifierFailed
  case class CursorFailed(cr: Failed) extends ModifierFailed
}

sealed trait ModifierResultInstances {

  implicit def monadInstanceForModifierResult: MonadError[ModifierResult, ModifierFailed] =
    new MonadError[ModifierResult, ModifierFailed] with StackSafeMonad[ModifierResult] {

      override def pure[A](x: A): ModifierResult[A] = ModifierResult.pure(x)

      override def raiseError[A](e: ModifierFailed): ModifierResult[A] = e

      override def handleErrorWith[A](fa: ModifierResult[A])(
        f: ModifierFailed => ModifierResult[A]
      ): ModifierResult[A] = fa match {
        case modified: ModifierResult.Modified[?] => modified.asInstanceOf[Modified[A]]
        case failed: ModifierFailed               => f(failed)
      }

      override def flatMap[A, B](fa: ModifierResult[A])(
        f: A => ModifierResult[B]
      ): ModifierResult[B] = fa.flatMap(f)
    }
}
