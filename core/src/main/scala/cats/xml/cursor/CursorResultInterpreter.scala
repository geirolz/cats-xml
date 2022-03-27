package cats.xml.cursor

import cats.{Alternative, MonadThrow}

trait CursorResultInterpreter[T] {
  def interpret(c: Cursor.Result[T]): Cursor.Result[T]
}
object CursorResultInterpreter extends CursorResultInterpreterInstances {

  def apply[T](implicit i: CursorResultInterpreter[T]): CursorResultInterpreter[T] = i

  def id[T]: CursorResultInterpreter[T] =
    CursorResultInterpreter.of(identity)

  def of[T](f: Cursor.Result[T] => Cursor.Result[T]): CursorResultInterpreter[T] =
    (c: Cursor.Result[T]) => f(c)
}

sealed trait CursorResultInterpreterInstances {

  implicit def CursorResultInterpreterForT[T]: CursorResultInterpreter[T] =
    CursorResultInterpreter.id

  implicit def CursorResultInterpreterForAlternative[F[_], T](implicit
    F: Alternative[F]
  ): CursorResultInterpreter[F[T]] =
    CursorResultInterpreter.of[F[T]] {
      case Right(value) => Right(value)
      case Left(_)      => Right(F.empty)
    }

  implicit def CursorResultInterpreterForMonadT[F[_], T](implicit
    F: MonadThrow[F]
  ): CursorResultInterpreter[F[T]] =
    CursorResultInterpreter.of[F[T]] {
      case Right(value)                 => Right(value)
      case Left(failure: CursorFailure) => Right(F.raiseError(failure.asException))
    }
}
