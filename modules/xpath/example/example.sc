import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error._
import cats.xml.xpath.implicits._
import cats.xml.XmlNode
import cats.xml.implicits._
import cats.implicits._

val cursor1: Either[XPathError, NodeCursor] = NodeCursor.fromXPath("/root[@id='1']")
val cursor2: Either[XPathError, NodeCursor] = xpath"/root[@id='1']"


val data = XmlNode("wrapper").withChildren(
  XmlNode("root").withAttributes("id" := 1)
)
val result: Either[Throwable, XmlNode] =
  cursor1
    .leftMapThrowable
    .flatMap(_.focus(data).leftMap(_.asException))