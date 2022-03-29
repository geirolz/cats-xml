package cats.xml.std

import cats.xml.std.codec.NodeSeqEncoderSyntax

object implicits extends AllInstances with AllSyntax

object instances extends AllInstances
sealed trait AllInstances extends NodeSeqConverterInstances

object syntax extends AllSyntax
sealed trait AllSyntax
    extends NodeSeqConverterSyntax
    with XmlAttributeConverterSyntax
    with XmlNormalizerSyntax
    with NodeSeqEncoderSyntax
