package cats.xml

object implicits extends AllSyntax

object syntax extends AllSyntax
private[xml] sealed trait AllSyntax extends XmlAttributeSyntax
