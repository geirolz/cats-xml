package cats.xml

import scala.util.{Success, Try}

class XmlPrinterSuite extends munit.FunSuite {

  import cats.xml.implicits.*

  test("XmlPrinter.prettyString convert XmlData to string") {

    val tree: Xml = XmlData.fromString("VALUE")

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(tree),
      expected = "VALUE"
    )
  }

  test("XmlPrinter.prettyString convert XmlAttribute to string") {

    val xml: Xml = XmlAttribute("KEY", "VALUE")

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(xml),
      expected = "KEY=\"VALUE\""
    )
  }

  test("XmlPrinter.prettyString convert XmlNull to string") {

    val xml: Xml = Xml.Null

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(xml),
      expected = ""
    )
  }

  test("XmlPrinter.prettyString convert simple XmlNode to well formatted XML string") {

    val xml: XmlNode =
      XmlNode("Foo")
        .withAttributes("A" := 10, "B" := true)
        .withText("Lorem ipsum dolor sit amet, Lorem ipsum dolor sit amet.")

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(xml),
      expected = """|<Foo A="10" B="true">
                    | Lorem ipsum dolor sit amet, Lorem ipsum dolor sit amet.
                    |</Foo>""".stripMargin
    )
  }

  test("XmlPrinter.prettyString convert nested XmlNode to well formatted XML string") {

    val xml: XmlNode = XmlNode("Foo").withChild(
      XmlNode("Bar")
        .withAttributes("F" := 'A')
        .withChild(
          XmlNode("Test")
            .withAttributes("G" := 100L)
            .withChild(
              XmlNode("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(xml),
      expected = """|<Foo>
                    | <Bar F="A">
                    |  <Test G="100">
                    |   <Node A="10" B="true">Lorem ipsum dolor sit amet</Node>
                    |  </Test>
                    | </Bar>
                    |</Foo>""".stripMargin
    )
  }

  test("XmlPrinter.prettyString convert XmlNodeGroup inside a node to well formatted XML string") {

    val xml: XmlNode =
      XmlNode("Foo").withChild(
        XmlNode.group(
          XmlNode("Bar").withAttributes("F" := "A"),
          XmlNode("Bar").withAttributes("F" := "B"),
          XmlNode("Bar").withAttributes("F" := "C")
        )
      )

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(xml),
      expected = """|<Foo>
                    | <Bar F="A"/>
                    | <Bar F="B"/>
                    | <Bar F="C"/>
                    |</Foo>""".stripMargin
    )
  }

  test("XmlPrinter.prettyString convert XmlNodeGroup to well formatted XML string") {

    val xml: XmlNode =
      XmlNode.group(
        XmlNode("Bar").withAttributes("F" := "A"),
        XmlNode("Bar").withAttributes("F" := "B"),
        XmlNode("Bar").withAttributes("F" := "C")
      )

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(xml),
      expected = """|<Bar F="A"/>
                    |<Bar F="B"/>
                    |<Bar F="C"/>""".stripMargin
    )
  }

  test("XmlPrinter.stringify convert xml tree to un-formatted string") {

    val tree: XmlNode = XmlNode("Foo").withChild(
      XmlNode("Bar")
        .withAttributes("F" := 'A')
        .withChild(
          XmlNode("Test")
            .withAttributes("G" := 100L)
            .withChild(
              XmlNode("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlPrinter.stringify(tree),
      expected =
        """<Foo><Bar F="A"><Test G="100"><Node A="10" B="true">Lorem ipsum dolor sit amet</Node></Test></Bar></Foo>""".stripMargin
    )
  }

  test("XmlPrinter.prettyString prints parsable XML") {

    val xml: XmlNode = XmlNode("Foo").withChild(
      XmlNode("Bar")
        .withAttributes("F" := 'A')
        .withChild(
          XmlNode("Test")
            .withAttributes("G" := 100L)
            .withChild(
              XmlNode("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlParser[Try].parseString(
        XmlPrinter.prettyString(xml)
      ),
      expected = Success(xml)
    )
  }
}
//
//class XmlPrinterPerformanceSuite extends munit.ScalaCheckSuite {
//
//  property("XmlPrinter.prettyString with XL document") {
//
//    implicit val arbXmlNode: Arbitrary[XmlNode] = Arbitrary(
//      XmlNodeGen.genXmlNode(DataSize.XL)
//    )
//
//    forAll { (value: XmlNode) =>
//      assert(
//        Ops.timed(XmlPrinter.prettyString(value))._1 < 1.seconds
//      )
//    }
//  }
//}
