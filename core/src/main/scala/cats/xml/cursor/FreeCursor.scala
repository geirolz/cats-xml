package cats.xml.cursor

import cats.{Monad, StackSafeMonad}
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.xml.{Xml, XmlNode}
import cats.xml.codec.{Decoder, DecoderFailure}

trait FreeCursor[I, +O] extends GenericCursor[I, O] with Serializable { $this =>

  def map[U](f: O => U): FreeCursor[I, U] =
    (in: I) => $this.focus(in).map(f)

  def flatMap[U](f: O => FreeCursor[I, U]): FreeCursor[I, U] =
    (in: I) => $this.focus(in).flatMap(f(_).focus(in))
}
object FreeCursor extends FreeCursorInstances {

  import cats.implicits.*

  def id[T]: FreeCursor[T, T] =
    _.asRight

  def const[I, O](result: Cursor.Result[O]): FreeCursor[I, O] =
    _ => result

  def apply[O: Decoder: CursorResultInterpreter](
    cursor: Cursor[Xml]
  ): FreeCursor[Xml, O] =
    new FreeCursor[Xml, O] { $this =>
      override def focus(xml: Xml): Cursor.Result[O] = {

        // TODO this smell
        val cursorResult: Cursor.Result[Xml] = xml match {
          case tree: XmlNode => cursor.focus(tree)
          case x: Xml        => x.asRight
        }

        CursorResultInterpreter[O].interpret(
          cursorResult.flatMap { x =>
            Decoder[O].decode(x) match {
              case Valid(value) => value.asRight
              case Invalid(failures: NonEmptyList[DecoderFailure]) =>
                CursorFailure.DecoderFailed(cursor.path, failures).asLeft
            }
          }
        )
      }
    }
}

sealed trait FreeCursorInstances {

  import cats.implicits.*

  implicit def monadFreeCursor[T]: Monad[FreeCursor[T, *]] = new StackSafeMonad[FreeCursor[T, *]] {

    override def pure[A](in: A): FreeCursor[T, A] =
      FreeCursor.const(in.asRight)

    override def flatMap[A, B](fa: FreeCursor[T, A])(f: A => FreeCursor[T, B]): FreeCursor[T, B] =
      fa.flatMap(f)
  }
}
