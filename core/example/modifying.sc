import cats.xml._
import cats.xml.modifier._
import scala.util.Try

val data = Xml.fromString[Try](
  """<wrapper>
    |  <root>
    |    <foo>
    |      <bar>
    |        <baz>
    |          <value>1</value>
    |        </baz>
    |      </bar>
    |    </foo>
    |  </root>
    |</wrapper>""".stripMargin).get

val result: Modifier.Result[XmlNode] =
  data.modify(_.root.foo.bar.baz.value.modifyIfNode(_.withText(2)))