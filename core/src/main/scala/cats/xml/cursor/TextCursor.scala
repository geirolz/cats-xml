package cats.xml.cursor

import cats.{Endo, Eq, Show}
import cats.data.Validated
import cats.xml.{XmlData, XmlNode}
import cats.xml.codec.{DataEncoder, Decoder}
import cats.xml.modifier.{Modifier, ModifierFailure}

/** Vertical cursor for node Text
  */
final class TextCursor(protected[xml] val lastCursor: NodeCursor)
    extends VCursor[XmlData, NodeCursor]
    with WithDataModifierSupport[XmlData] {
  $this =>

  import cats.implicits.*

  override lazy val path: String = lastCursor.path

  // modify
  override def modify(modifier: Endo[XmlData]): Modifier[XmlNode] =
    modify[XmlData, XmlData](modifier)

  override def modify[T: Decoder, U: DataEncoder](f: T => U): Modifier[XmlNode] =
    Modifier(node =>
      $this.as[T].focus(node) match {
        case Validated.Valid(textValue) =>
          lastCursor.modifyNode(_.withText(f(textValue)))(node)
        case Validated.Invalid(failures) =>
          ModifierFailure.CursorFailed(failures).asLeft
      }
    )

  // focus
  override def focus(node: XmlNode): Cursor.Result[XmlData] =
    lastCursor
      .focus(node)
      .map(_.text)
      .flatMap {
        case Some(value: XmlData) => Right(value)
        case None                 => Left(CursorFailure.MissingText(lastCursor.path))
      }

  // eq
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[TextCursor] &&
      Eq[TextCursor].eqv(this, obj.asInstanceOf[TextCursor])
}
object TextCursor {

  // instances
  implicit val eq: Eq[TextCursor] = (x: TextCursor, y: TextCursor) =>
    x.lastCursor.equals(y.lastCursor)

  implicit val show: Show[TextCursor] = Show.fromToString
}
