package cats.xml

import cats.xml.testing.{DataSize, Ops, XmlNodeGen}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.concurrent.duration.DurationInt
import scala.util.{Success, Try}

class XmlPrinterSuite extends munit.FunSuite {

  import cats.implicits.*
  import cats.xml.implicits.*

  test("XmlPrinter.default.prettyString convert XmlData to string") {

    val tree: Xml = XmlData.fromString("VALUE")

    // assert
    assertEquals(
      obtained = XmlPrinter.default.prettyString(tree),
      expected = "VALUE"
    )
  }

  test("XmlPrinter.default.prettyString convert XmlAttribute to string") {

    val xml: Xml = XmlAttribute("KEY", "VALUE")

    // assert
    assertEquals(
      obtained = XmlPrinter.default.prettyString(xml),
      expected = "KEY=\"VALUE\""
    )
  }

  test("XmlPrinter.default.prettyString convert XmlNull to string") {

    val xml: Xml = Xml.Null

    // assert
    assertEquals(
      obtained = XmlPrinter.default.prettyString(xml),
      expected = ""
    )
  }

  test("XmlPrinter.default.prettyString convert simple XmlNode to well formatted XML string") {

    val xml: XmlNode =
      XmlNode("Foo")
        .withAttributes("A" := 10, "B" := true)
        .withText("Lorem ipsum dolor sit amet, Lorem ipsum dolor sit amet.")

    // assert
    assertEquals(
      obtained = XmlPrinter.default.prettyString(xml),
      expected = """|<Foo A="10" B="true">
                    | Lorem ipsum dolor sit amet, Lorem ipsum dolor sit amet.
                    |</Foo>""".stripMargin
    )
  }

  test("XmlPrinter.default.prettyString convert nested XmlNode to well formatted XML string") {

    val xml: XmlNode = XmlNode("Foo").withChildren(
      XmlNode("Bar")
        .withAttributes("F" := 'A')
        .withChildren(
          XmlNode("Test")
            .withAttributes("G" := 100L)
            .withChildren(
              XmlNode("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlPrinter.default.prettyString(xml),
      expected = """|<Foo>
                    | <Bar F="A">
                    |  <Test G="100">
                    |   <Node A="10" B="true">Lorem ipsum dolor sit amet</Node>
                    |  </Test>
                    | </Bar>
                    |</Foo>""".stripMargin
    )
  }

  test(
    "XmlPrinter.default.prettyString convert XmlNodeGroup inside a node to well formatted XML string"
  ) {

    val xml: XmlNode =
      XmlNode("Foo").withChildren(
        XmlNode.group(
          XmlNode("Bar").withAttributes("F" := "A"),
          XmlNode("Bar").withAttributes("F" := "B"),
          XmlNode("Bar").withAttributes("F" := "C")
        )
      )

    // assert
    assertEquals(
      obtained = XmlPrinter.default.prettyString(xml),
      expected = """|<Foo>
                    | <Bar F="A"/>
                    | <Bar F="B"/>
                    | <Bar F="C"/>
                    |</Foo>""".stripMargin
    )
  }

  test("XmlPrinter.default.prettyString convert XmlNodeGroup to well formatted XML string") {

    val xml: XmlNode =
      XmlNode.group(
        XmlNode("Bar").withAttributes("F" := "A"),
        XmlNode("Bar").withAttributes("F" := "B"),
        XmlNode("Bar").withAttributes("F" := "C")
      )

    // assert
    assertEquals(
      obtained = XmlPrinter.default.prettyString(xml),
      expected = """|<Bar F="A"/>
                    |<Bar F="B"/>
                    |<Bar F="C"/>""".stripMargin
    )
  }

  test("XmlPrinter.stringify convert xml tree to un-formatted string") {

    val tree: XmlNode = XmlNode("Foo").withChildren(
      XmlNode("Bar")
        .withAttributes("F" := 'A')
        .withChildren(
          XmlNode("Test")
            .withAttributes("G" := 100L)
            .withChildren(
              XmlNode("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlPrinter.default.stringify(tree),
      expected =
        """<Foo><Bar F="A"><Test G="100"><Node A="10" B="true">Lorem ipsum dolor sit amet</Node></Test></Bar></Foo>""".stripMargin
    )
  }

  test("XmlPrinter.default.prettyString prints parsable XML") {

    val xml: XmlNode = XmlNode("Foo").withChildren(
      XmlNode("Bar")
        .withAttributes("F" := 'A')
        .withChildren(
          XmlNode("Test")
            .withAttributes("G" := 100L)
            .withChildren(
              XmlNode("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlParser[Try]
        .parseString(
          XmlPrinter.default.prettyString(xml)
        )
        .map(_.show),
      expected = Success(xml.show)
    )
  }
}

class XmlPrinterPerformanceSuite extends munit.ScalaCheckSuite {

  property("XmlPrinter.default.prettyString with XL document") {

    implicit val arbXmlNode: Arbitrary[XmlNode] = Arbitrary(
      XmlNodeGen.genXmlNode(DataSize.L)
    )

    forAll { (value: XmlNode) =>
      assert(
        Ops.timed(XmlPrinter.default.prettyString(value))._1 < 30.millis
      )
    }
  }
}
