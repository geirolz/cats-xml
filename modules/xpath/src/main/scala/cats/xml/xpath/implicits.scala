package cats.xml.xpath

import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error.XPathError
import eu.cdevreeze.xpathparser.ast.XPathExpr

import scala.util.Try

object implicits {

  implicit class XmlNodeCursorOps(xml: NodeCursor.type) { $this =>

    def fromXPath(xpathExpr: XPathExpr): Either[XPathError, NodeCursor] =
      CursorBuilder.fromXPath(xpathExpr)

    def fromXPath(xpathValue: String): Either[XPathError, NodeCursor] =
      CursorBuilder.fromXPath(xpathValue)
  }

  implicit class XmlNodeCursorFromXPathStrContext(ctx: StringContext) {

    // TODO USE MACRO
    def xpath(args: Any*): Either[XPathError, NodeCursor] =
      NodeCursor.fromXPath(ctx.s(args*))
  }

  implicit class XmlXPathResultOps(value: Either[XPathError, NodeCursor]) {

    def leftMapThrowable: Either[Throwable, NodeCursor] =
      value.left.map(_.toException)

    def toTryValue: Try[NodeCursor] =
      value.left.map(_.toException).toTry
  }
}
