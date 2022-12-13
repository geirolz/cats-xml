package cats.xml.cursor

import cats.xml.{XmlData, XmlNode}
import cats.xml.codec.DataEncoder
import cats.xml.modifier.{Modifier, ModifierFailure}
import cats.{Eq, Show}

/** Vertical cursor for node Text
  */
final class TextCursor(protected[xml] val lastCursor: NodeCursor)
    extends VCursor[XmlData, NodeCursor] {
  $this =>

  import cats.implicits.*

  override lazy val path: String = lastCursor.path

  // modify
  def modify[T: DataEncoder](f: XmlData => T): Modifier[XmlNode] =
    Modifier(node =>
      $this.focus(node) match {
        case Right(textValue) =>
          lastCursor.modifyIfNode(_.withText(f(textValue)))(node)
        case Left(failure) =>
          ModifierFailure.CursorFailed(failure).asLeft
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
    obj.isInstanceOf[TextCursor]
      && Eq[TextCursor].eqv(this, obj.asInstanceOf[TextCursor])
}
object TextCursor {

  // instances
  implicit val eq: Eq[TextCursor] = (x: TextCursor, y: TextCursor) =>
    x.lastCursor.equals(y.lastCursor)

  implicit val show: Show[TextCursor] = Show.fromToString
}
