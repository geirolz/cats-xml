package cats.xml.std

import cats.xml.std.codec.NodeSeqEncoderSyntax

object implicits extends AllSyntax

sealed trait AllSyntax
    extends XmlToNodeSeqSyntax
    with XmlAttributeInteropSyntax
    with NodeSeqEncoderSyntax
