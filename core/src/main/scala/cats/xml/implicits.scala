package cats.xml

import cats.xml.codec.{DecoderFailureSyntax, DecoderSyntax, EncoderSyntax}

object implicits extends AllSyntax

object syntax extends AllSyntax
private[xml] sealed trait AllSyntax
    extends XmlAttributeSyntax
    with DecoderFailureSyntax
    with XmlParserSyntax
    with EncoderSyntax
    with DecoderSyntax
