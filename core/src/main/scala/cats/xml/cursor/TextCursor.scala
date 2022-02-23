package cats.xml.cursor

import cats.xml.{XmlData, XmlNode}

/** Vertical cursor for node Text
  */
class TextCursor(protected val lastCursor: NodeCursor) extends VCursor[XmlData, NodeCursor] {

  override lazy val path: String = s"${lastCursor.path} | text"

  // focus
  override def focus(node: XmlNode): CursorResult[XmlData] =
    lastCursor
      .focus(node)
      .map(_.text)
      .flatMap {
        case Some(value: XmlData) => CursorResult.Focused(value)
        case None                 => CursorResult.MissingText(lastCursor.path)
      }
}
