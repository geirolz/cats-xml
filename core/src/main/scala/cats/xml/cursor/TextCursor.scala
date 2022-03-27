package cats.xml.cursor

import cats.xml.{XmlData, XmlNode}
import cats.xml.codec.DataEncoder
import cats.xml.modifier.Modifier

/** Vertical cursor for node Text
  */
class TextCursor(protected[xml] val lastCursor: NodeCursor) extends VCursor[XmlData, NodeCursor] {

  override lazy val path: String = lastCursor.path

  // modify
  def modify[T: DataEncoder](f: Option[XmlData] => T): Modifier[XmlNode] =
    Modifier.fromTextCursor(this, f)

  // focus
  override def focus(node: XmlNode): Cursor.Result[XmlData] =
    lastCursor
      .focus(node)
      .map(_.text)
      .flatMap {
        case Some(value: XmlData) => Right(value)
        case None                 => Left(CursorFailure.MissingText(lastCursor.path))
      }
}
