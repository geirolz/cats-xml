package cats.xml.xpath

import cats.xml.cursor.{Cursor, CursorFailure, NodeCursor}
import cats.xml.xpath.error.XPathError
import cats.xml.XmlNode
import eu.cdevreeze.xpathparser.ast.XPathExpr

import scala.util.Try

object implicits {

  implicit class XmlNodeOps(xml: XmlNode) {
    def xpath(xpathStr: String): Cursor.Result[XmlNode] =
      CursorBuilder.fromXPath(xpathStr).leftMapCursorFailure.flatMap(_.focus(xml))

    def xpath(xpathExpr: XPathExpr): Cursor.Result[XmlNode] =
      CursorBuilder.fromXPath(xpathExpr).leftMapCursorFailure.flatMap(_.focus(xml))
  }

  implicit class XmlNodeCursorOps(ncObj: NodeCursor.type) {

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

    def leftMapCursorFailure: Either[CursorFailure, NodeCursor] =
      leftMapThrowable.left.map(CursorFailure.Error(_))

    def toTryValue: Try[NodeCursor] =
      value.left.map(_.toException).toTry
  }
}
