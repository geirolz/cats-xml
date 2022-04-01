package cats.xml

class XmlPrinterSuite extends munit.FunSuite {

  import cats.xml.implicits.*

  test("XmlPrinter.prettyString convert xml tree to well formatted XML string") {

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
      obtained = XmlPrinter.prettyString(tree),
      expected = """|<Foo>
                    | <Bar F="A">
                    |  <Test G="100">
                    |   <Node A="10" B="true">Lorem ipsum dolor sit amet</Node>
                    |  </Test>
                    | </Bar>
                    |</Foo>""".stripMargin
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
}

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
