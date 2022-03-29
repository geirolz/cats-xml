package cats.xml

import scala.util.{Success, Try}

class XmlParserSuite extends munit.FunSuite {

  import cats.xml.implicits.*

  test("XmlParser.parseString") {
    assertEquals(
      obtained = XmlParser[Try].parseString(
        """
          |<Foo intAttr="1" boolAttr="true">
          |    <Bar intAttr="2" emptyAttr="">
          |        <Baz>100</Baz>
          |    </Bar>
          |</Foo>""".stripMargin
      ),
      expected = Success(
        XmlNode("Foo")
          .withAttributes(
            "intAttr"  := 1,
            "boolAttr" := true
          )
          .withChild(
            XmlNode("Bar")
              .withAttributes(
                "intAttr"   := 2,
                "emptyAttr" := ""
              )
              .withChild(
                XmlNode("Baz").withText(100)
              )
          )
      )
    )
  }

  test("XmlParser.parseString with String interpolation") {
    assertEquals(
      obtained = xml"""
           <Foo intAttr="1" boolAttr="true">
              <Bar intAttr="2" emptyAttr="">
                  <Baz>100</Baz>
              </Bar>
          </Foo>""",
      expected = Success(
        XmlNode("Foo")
          .withAttributes(
            "intAttr"  := 1,
            "boolAttr" := true
          )
          .withChild(
            XmlNode("Bar")
              .withAttributes(
                "intAttr"   := 2,
                "emptyAttr" := ""
              )
              .withChild(
                XmlNode("Baz").withText(100)
              )
          )
      )
    )
  }

}
