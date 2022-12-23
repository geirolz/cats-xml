import cats.xml.Xml
import cats.xml.XmlNode
import cats.xml.cursor.Cursor
import cats.xml.cursor.FreeCursor
import scala.util.Try

val node = Xml.fromString[Try](
  """<wrapper>
    |    <root>
    |        <foo>1</foo>
    |        <bar>2</bar>
    |        <baz>3</baz>
    |    </root>
    |</wrapper>""".stripMargin).get


val fooNode: Cursor.Result[XmlNode] = node.focus(_.root.foo)
val fooTextValue: FreeCursor.Result[Int] = node.focus(_.root.foo.text.as[Int])