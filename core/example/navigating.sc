import cats.xml.XmlNode
import cats.xml.cursor.Cursor
import cats.xml.cursor.FreeCursor
import cats.xml.implicits._

val node = xml"""
     <wrapper>
        <root>
            <foo>1</foo>
            <bar>2</bar>
            <baz>2</baz>
        </root>
    </wrapper>"""

val fooNode: Cursor.Result[XmlNode]      = node.focus(_.root.foo)
val fooTextValue: FreeCursor.Result[Int] = node.focus(_.root.foo.text.as[Int])
