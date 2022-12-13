package cats.xml.std

import cats.xml.{Xml, XmlNode}
import cats.xml.std.testing.NodeSeqAssertions

class NodeSeqConverterSuite extends munit.FunSuite with NodeSeqAssertions {

  import cats.xml.implicits.*
  import cats.xml.std.implicits.*

  test("NodeSeqConverter.fromNodeSeq") {
    assertEquals(
      obtained = NodeSeqConverter
        .fromNodeSeq(
          <Foo intAttr="1" boolAttr="true">
          <Bar intAttr="2" emptyAttr="">
            <Baz>100</Baz>
          </Bar>
        </Foo>
        ),
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
              XmlNode("Baz").withText(100)
            )
        )
    )
  }

  test("Xml.fromNodeSeq") {

    assertEquals(
      obtained = Xml.fromNodeSeq(
        <Foo intAttr="1" boolAttr="true">
          <Bar intAttr="2" emptyAttr="">
            <Baz>100</Baz>
          </Bar>
        </Foo>
      ),
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
              XmlNode("Baz").withText(100)
            )
        )
    )
  }

  test("NodeSeq.toXmlNode") {
    assertEquals(
      obtained = <Foo intAttr="1" boolAttr="true">
          <Bar intAttr="2" emptyAttr="">
            <Baz>100</Baz>
          </Bar>
        </Foo>.toXmlNode,
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
              XmlNode("Baz").withText(100)
            )
        )
    )
  }

  test("implicit conversion from NodeSeq to XmlNode") {
    assertEquals[XmlNode, XmlNode](
      obtained = <Foo intAttr="1" boolAttr="true">
          <Bar intAttr="2" emptyAttr="">
            <Baz>100</Baz>
          </Bar>
        </Foo>,
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
              XmlNode("Baz").withText(100)
            )
        )
    )
  }

  test("NodeSeqConverter.toNodeSeq") {
    assertEqualsNodeSeq(
      obtained = NodeSeqConverter
        .toNodeSeq(
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
                  XmlNode("Baz").withText(100)
                )
            )
        ),
      expected = <Foo intAttr="1" boolAttr="true">
        <Bar intAttr="2" emptyAttr="">
          <Baz>100</Baz>
        </Bar>
      </Foo>
    )
  }

  test("Xml.toNodeSeq") {
    assertEqualsNodeSeq(
      obtained = Xml.toNodeSeq(
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
                XmlNode("Baz").withText(100)
              )
          )
      ),
      expected = <Foo intAttr="1" boolAttr="true">
        <Bar intAttr="2" emptyAttr="">
          <Baz>100</Baz>
        </Bar>
      </Foo>
    )
  }
}
