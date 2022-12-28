package cats.xml

import cats.xml.codec.{DecoderFailureSyntax, DecoderSyntax, EncoderSyntax}
import cats.xml.utils.DebugSyntax

object implicits extends AllSyntax

object syntax extends AllSyntax
private[xml] sealed trait AllSyntax
    extends XmlAttributeSyntax
    with DecoderFailureSyntax
    with XmlParserSyntax
    with EncoderSyntax
    with DecoderSyntax
    with DebugSyntax {

  implicit class XmlDataOptionOps[X <: Xml](opt: Option[X]) {
    def orXmlNull: X = opt.getOrElse(Xml.Null).asInstanceOf[X]
  }
}
