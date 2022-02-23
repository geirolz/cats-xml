package cats.xml.cursor

import cats.{Alternative, MonadThrow}

trait CursorResultInterpreter[T] {
  def interpret(c: CursorResult[T]): CursorResult[T]
}
object CursorResultInterpreter extends CursorResultInterpreterInstances {

  def apply[T](implicit i: CursorResultInterpreter[T]): CursorResultInterpreter[T] = i

  def id[T]: CursorResultInterpreter[T] =
    CursorResultInterpreter.of(identity)

  def of[T](f: CursorResult[T] => CursorResult[T]): CursorResultInterpreter[T] =
    (c: CursorResult[T]) => f(c)
}

sealed trait CursorResultInterpreterInstances {

  implicit def CursorResultInterpreterForT[T]: CursorResultInterpreter[T] =
    CursorResultInterpreter.id

  implicit def CursorResultInterpreterForAlternative[F[_], T](implicit
    F: Alternative[F]
  ): CursorResultInterpreter[F[T]] =
    CursorResultInterpreter.of[F[T]] {
      case CursorResult.Focused(value) => CursorResult.Focused(value)
      case _: CursorResult.Failed      => CursorResult.Focused(F.empty)
    }

  implicit def CursorResultInterpreterForMonadT[F[_], T](implicit
    F: MonadThrow[F]
  ): CursorResultInterpreter[F[T]] =
    CursorResultInterpreter.of[F[T]] {
      case CursorResult.Focused(value) => CursorResult.Focused(value)
      case e: CursorResult.Failed      => CursorResult.Focused(F.raiseError(e.asException))
    }
}
