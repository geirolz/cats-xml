package cats.xml

import scala.util.{Success, Try}

class XmlParserSuite extends munit.FunSuite {

  import cats.xml.implicits.*

  test("XmlParser.parseString") {
    assertEquals(
      obtained = XmlParser[Try].parseString(
        """
          |<Foo intAttr="1" boolAttr="true">
          |   <Bar intAttr="2" emptyAttr="">
          |     <Baz>100</Baz>
          |     <Group>
          |       <A>1</A>
          |       <B>2</B>
          |       <C>3</C>
          |     </Group>
          |     <Group>
          |       <A>4</A>
          |       <B>5</B>
          |       <C>6</C>
          |     </Group>
          |   </Bar>
          |</Foo>""".stripMargin
      ),
      expected = Success(
        XmlNode("Foo")
          .withAttributes(
            "intAttr"  := 1,
            "boolAttr" := true
          )
          .withChildren(
            XmlNode("Bar")
              .withAttributes(
                "intAttr"   := 2,
                "emptyAttr" := ""
              )
              .withChildren(
                XmlNode("Baz").withText(100),
                XmlNode("Group").withChildren(
                  XmlNode("A").withText(1),
                  XmlNode("B").withText(2),
                  XmlNode("C").withText(3)
                ),
                XmlNode("Group").withChildren(
                  XmlNode("A").withText(4),
                  XmlNode("B").withText(5),
                  XmlNode("C").withText(6)
                )
              )
          )
      )
    )
  }

  test("XmlParser.parseString with String interpolation") {
    assertEquals(
      obtained = Xml
        .fromString[Try]("""<Foo intAttr="1" boolAttr="true">
          |    <Bar intAttr="2" emptyAttr="">
          |        <Baz>100</Baz>
          |        <Group>
          |          <A>1</A>
          |          <B>2</B>
          |          <C>3</C>
          |        </Group>
          |        <Group>
          |          <A>4</A>
          |          <B>5</B>
          |          <C>6</C>
          |        </Group>
          |    </Bar>
          |</Foo>""".stripMargin)
        .get,
      expected = XmlNode("Foo")
        .withAttributes(
          "intAttr"  := 1,
          "boolAttr" := true
        )
        .withChildren(
          XmlNode("Bar")
            .withAttributes(
              "intAttr"   := 2,
              "emptyAttr" := ""
            )
            .withChildren(
              XmlNode("Baz").withText(100),
              XmlNode("Group").withChildren(
                XmlNode("A").withText(1),
                XmlNode("B").withText(2),
                XmlNode("C").withText(3)
              ),
              XmlNode("Group").withChildren(
                XmlNode("A").withText(4),
                XmlNode("B").withText(5),
                XmlNode("C").withText(6)
              )
            )
        )
    )
  }
}
