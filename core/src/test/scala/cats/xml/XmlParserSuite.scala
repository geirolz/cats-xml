package cats.xml

import cats.data.Validated.Valid
import cats.xml.codec.Decoder

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
          .withAttrs(
            "intAttr"  := 1,
            "boolAttr" := true
          )
          .withChildren(
            XmlNode("Bar")
              .withAttrs(
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
      obtained = xml"""
           <Foo intAttr="1" boolAttr="true">
              <Bar intAttr="2" emptyAttr="">
                  <Baz>100</Baz>
                  <Group>
                    <A>1</A>
                    <B>2</B>
                    <C>3</C>
                  </Group>
                  <Group>
                    <A>4</A>
                    <B>5</B>
                    <C>6</C>
                  </Group>
              </Bar>
          </Foo>""",
      expected = XmlNode("Foo")
        .withAttrs(
          "intAttr"  := 1,
          "boolAttr" := true
        )
        .withChildren(
          XmlNode("Bar")
            .withAttrs(
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

  test("XmlParser.parseString with long number that should be parsed as BigDecimal") {
    val xml: XmlNode = xml"""<Data value="5340595900475325933418219074917123456789123456789"/>"""

    assertEquals(
      obtained = xml,
      expected = XmlNode("Data")
        .withAttrs(
          "value" := BigDecimal("5340595900475325933418219074917123456789123456789")
        )
    )
  }

  test("XmlParser preserve zero in front of number") {

    val x =
      xml"""
        <foo>
          <bar>01</bar>
        </foo>
      """

    case class Bar(v: String)

    implicit val barDecoder: Decoder[Bar] = Decoder.fromCursor { c =>
      c.down("bar")
        .as[String]
        .map(Bar.apply)
    }

    assertEquals(
      obtained = x.as[Bar],
      expected = Valid(Bar("01"))
    )
  }
}
