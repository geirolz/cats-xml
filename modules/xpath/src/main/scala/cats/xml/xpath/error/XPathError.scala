package cats.xml.xpath.error

import cats.parse.Parser
import cats.xml.xpath.error.XPathError.XPathErrorException
import eu.cdevreeze.xpathparser.ast.XPathElem

sealed trait XPathError {

  def message: String

  override final def toString: String =
    this.message

  final def toException: XPathErrorException =
    new XPathErrorException(this.message)
}
object XPathError {
  final case class ParsingError(err: Parser.Error) extends XPathError {
    override def message: String =
      s"Parsing failed at ${err.failedAtOffset}: ${err.expected.toList.mkString}."
  }
  final case class NotSupportedConstruction(feature: XPathElem) extends XPathError {
    override def message: String =
      s"Not supported construction for feature $feature."
  }

  class XPathErrorException(msg: String)
      extends RuntimeException(
        s"XPath Error: $msg"
      )
}
