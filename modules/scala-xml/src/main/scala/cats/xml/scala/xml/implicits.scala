package cats.xml.scala.xml

import cats.xml.scala.xml.codec.NodeSeqEncoderSyntax

object implicits extends AllSyntax

sealed trait AllSyntax
    extends XmlToNodeSeqSyntax
    with XmlAttributeInteropSyntax
    with NodeSeqEncoderSyntax
