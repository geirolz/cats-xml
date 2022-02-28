package cats.xml.cursor

import cats.data.Validated.{Invalid, Valid}
import cats.xml.{Xml, XmlNode}
import cats.xml.codec.Decoder
import cats.{Monad, StackSafeMonad}

trait FreeCursor[I, +O] extends GenericCursor[I, O] with Serializable { $this =>

  def map[U](f: O => U): FreeCursor[I, U] =
    (in: I) => $this.focus(in).map(f)

  def flatMap[U](f: O => FreeCursor[I, U]): FreeCursor[I, U] =
    (in: I) =>
      $this.focus(in) match {
        case CursorResult.Focused(value) => f(value).focus(in)
        case failed: CursorResult.Failed => failed
      }
}
object FreeCursor extends FreeCursorInstances {

  def id[T]: FreeCursor[T, T] =
    (i: T) => CursorResult.Focused(i)

  def const[I, O](result: CursorResult[O]): FreeCursor[I, O] =
    _ => result

  def apply[O: Decoder: CursorResultInterpreter](
    cursor: Cursor[Xml]
  ): FreeCursor[Xml, O] =
    new FreeCursor[Xml, O] { $this =>
      override def focus(xml: Xml): CursorResult[O] = {

        // TODO this smell
        val cursorResult: CursorResult[Xml] = xml match {
          case tree: XmlNode => cursor.focus(tree)
          case x             => CursorResult.Focused(x)
        }

        CursorResultInterpreter[O].interpret(
          cursorResult.flatMap { x =>
            Decoder[O].decode(x) match {
              case Valid(a)       => CursorResult.Focused(a)
              case e @ Invalid(_) => CursorResult.CursorDecodingFailure(cursor.path, e)
            }
          }
        )
      }
    }
}

sealed trait FreeCursorInstances {

  implicit def monadFreeCursor[T]: Monad[FreeCursor[T, *]] = new StackSafeMonad[FreeCursor[T, *]] {

    override def pure[A](in: A): FreeCursor[T, A] =
      FreeCursor.const(CursorResult.Focused(in))

    override def flatMap[A, B](fa: FreeCursor[T, A])(f: A => FreeCursor[T, B]): FreeCursor[T, B] =
      fa.flatMap(f)
  }
}
