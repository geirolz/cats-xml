package cats.xml

import cats.xml.codec.DecoderFailureSyntax

object implicits extends AllSyntax

object syntax extends AllSyntax
private[xml] sealed trait AllSyntax
    extends XmlAttributeSyntax
    with DecoderFailureSyntax
    with XmlParserSyntax
