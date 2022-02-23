package cats.xml.cursor

import cats.xml.{Xml, XmlNode}
import cats.xml.codec.Decoder
import cats.data.Validated.{Invalid, Valid}

trait FreeCursor[+T] extends GenericCursor[Xml, T] with Serializable { $this =>

  def map[U](f: T => U): FreeCursor[U] =
    (xml: Xml) => $this.focus(xml).map(f)

  def flatMap[U](f: T => FreeCursor[U]): FreeCursor[U] =
    (xml: Xml) =>
      $this.focus(xml) match {
        case CursorResult.Focused(value) => f(value).focus(xml)
        case failed: CursorResult.Failed => failed
      }
}
object FreeCursor {

  def apply[T: Decoder: CursorResultInterpreter](
    cursor: Cursor[Xml]
  ): FreeCursor[T] =
    new FreeCursor[T] { $this =>
      override def focus(xml: Xml): CursorResult[T] = {

        // TODO this smell
        val cursorResult: CursorResult[Xml] = xml match {
          case tree: XmlNode => cursor.focus(tree)
          case x             => CursorResult.Focused(x)
        }

        CursorResultInterpreter[T].interpret(
          cursorResult.flatMap { x =>
            Decoder[T].decode(x) match {
              case Valid(a)       => CursorResult.Focused(a)
              case e @ Invalid(_) => CursorResult.DecodingFailure(cursor.path, e)
            }
          }
        )
      }
    }
}
