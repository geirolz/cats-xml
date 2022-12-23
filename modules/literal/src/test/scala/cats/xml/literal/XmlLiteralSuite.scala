package cats.xml.literal

import cats.xml.XmlNode

class XmlLiteralSuite extends munit.FunSuite {

  import cats.xml.literal.Implicits.*

  test("XML Literal with string expression") {

    val node: XmlNode =xml"""<wrapper>
              <root>
                  <foo>1</foo>
                  <bar>2</bar>
                  <baz>3</baz>
              </root>
            </wrapper>"""

    assertEquals(
      obtained = node,
      expected = XmlNode("wrapper").withChildren(
        XmlNode("root").withChildren(
          XmlNode("foo").withText(1),
          XmlNode("bar").withText(2),
          XmlNode("baz").withText(3)
        )
      )
    )
  }
}
