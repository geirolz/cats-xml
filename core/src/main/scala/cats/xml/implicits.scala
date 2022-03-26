package cats.xml

import cats.xml.cursor.CursorResultSyntax
import cats.xml.modifier.ModifierResultSyntax

object implicits extends AllSyntax

object syntax extends AllSyntax
private[xml] sealed trait AllSyntax
    extends XmlAttributeSyntax
    with CursorResultSyntax
    with ModifierResultSyntax
