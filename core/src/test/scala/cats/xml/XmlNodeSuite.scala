package cats.xml

class XmlNodeSuite extends munit.FunSuite {

  import cats.xml.syntax.*

  test("Empty XmlNode flags") {
    val emptyNode: XmlNode = XmlNode("Foo")

    // is/has
    assert(emptyNode.isNode)
    assert(emptyNode.isEmpty)
    assert(emptyNode.asNode.isDefined)
    assert(emptyNode.asData.isEmpty)
    assert(emptyNode.asAttribute.isEmpty)

    // not is/ not has
    assert(!emptyNode.isGroup)
    assert(!emptyNode.isData)
    assert(!emptyNode.isAttribute)
    assert(!emptyNode.isNull)
    assert(!emptyNode.hasText)
  }

  test("XmlNode flags") {
    val node: XmlNode = XmlNode("Foo").withText("Text")

    // is/has
    assert(node.isNode)
    assert(node.asNode.isDefined)
    assert(node.asData.isEmpty)
    assert(node.asAttribute.isEmpty)
    assert(node.hasText)
    assertEquals(
      obtained = node.textString,
      expected = "Text"
    )

    // not is/ not has
    assert(!node.isGroup)
    assert(!node.isData)
    assert(!node.isAttribute)
    assert(!node.isNull)
    assert(!node.isEmpty)
  }

  test("XmlNode.apply") {
    assertEquals(
      obtained = XmlNode("Foo", List("A" := 1), NodeContent.textOrEmpty("Text")),
      expected = XmlNode("Foo").withAttributes("A" := 1).withText("Text")
    )
    intercept[IllegalArgumentException](
      body = XmlNode("")
    )
    intercept[IllegalArgumentException](
      body = XmlNode(null)
    )
    intercept[IllegalArgumentException](
      body = XmlNode("Foo", null)
    )
    intercept[IllegalArgumentException](
      body = XmlNode("Foo", List(), null)
    )
  }

  // ################### LABEL ###################
  test("XmlNode.updateLabel value") {
    assertEquals(
      XmlNode("Foo")
        .withAttributes("A" := 1)
        .withText("Text")
        .withLabel("Bar"),
      XmlNode("Bar")
        .withAttributes("A" := 1)
        .withText("Text")
    )

    intercept[IllegalArgumentException](
      XmlNode("Foo").withLabel("")
    )
  }

  test("XmlNode.updateLabel function") {
    assertEquals(
      XmlNode("Foo")
        .withAttributes("A" := 1)
        .withText("Text")
        .updateLabel(_ ++ "Bar"),
      XmlNode("FooBar")
        .withAttributes("A" := 1)
        .withText("Text")
    )
    intercept[IllegalArgumentException](
      XmlNode("Foo").updateLabel(_ => "")
    )
    intercept[IllegalArgumentException](
      XmlNode("Foo").updateLabel(_ => null)
    )
  }

  // ################### ATTRS ###################
  test("XmlNode.findAttrRaw") {
    val node = XmlNode("Foo").withAttributes(
      "A" := 1,
      "B" := 2,
      "C" := 3
    )

    assertEquals(
      obtained = node.findAttrRaw("B"),
      expected = Some(XmlAttribute("B", 2))
    )
    assertEquals(
      obtained = node.findAttrRaw("D"),
      expected = None
    )
  }

  test("XmlNode.findAttr") {
    val node = XmlNode("Foo").withAttributes(
      "A" := 1,
      "B" := 2,
      "C" := 3
    )

    assertEquals(
      obtained = node.findAttr[Int]("B"),
      expected = Some(2)
    )
    assertEquals(
      obtained = node.findAttr[Int]("D"),
      expected = None
    )
  }

  test("XmlNode.findAttrWhere") {
    val node = XmlNode("Foo").withAttributes(
      "A" := 1,
      "B" := 2,
      "C" := 3
    )

    assertEquals(
      obtained = node.findAttrWhere[Int](_ == "B", _ < 5),
      expected = Some(2)
    )
    assertEquals(
      obtained = node.findAttrWhere[Int](_ == "B", _ > 5),
      expected = None
    )
    assertEquals(
      obtained = node.findAttrWhere[Int](_ == "D", _ == 4),
      expected = None
    )
  }

  test("XmlNode.existsAttrByKey") {
    val node = XmlNode("Foo").withAttributes(
      "A" := 1,
      "B" := 2,
      "C" := 3
    )

    assertEquals(
      obtained = node.existsAttrByKey(_ == "B"),
      expected = true
    )
    assertEquals(
      obtained = node.existsAttrByKey(_ == "D"),
      expected = false
    )
  }

  test("XmlNode.existsAttrWithValue") {
    val node = XmlNode("Foo").withAttributes(
      "A" := 1,
      "B" := 2,
      "C" := 3
    )

    assertEquals(
      obtained = node.existsAttrWithValue[Int]("B", _ == 2),
      expected = true
    )
    assertEquals(
      obtained = node.existsAttrWithValue[Int]("B", _ == 4),
      expected = false
    )
    assertEquals(
      obtained = node.existsAttrWithValue[Int]("D", _ == 4),
      expected = false
    )
  }

  test("XmlNode.existsAttrWithValue") {
    val node = XmlNode("Foo").withAttributes(
      "A" := 1,
      "B" := 2,
      "C" := 3
    )

    assertEquals(
      obtained = node.existsAttrByKeyAndValue[Int](_ == "B", _ == 2),
      expected = true
    )
    assertEquals(
      obtained = node.existsAttrByKeyAndValue[Int](_ == "B", _ == 4),
      expected = false
    )
    assertEquals(
      obtained = node.existsAttrByKeyAndValue[Int](_ == "D", _ == 4),
      expected = false
    )
  }

  // ----------- appendAttrs -----------
  test("XmlNode.appendAttr preserve normalization") {
    assertEquals(
      obtained = XmlNode("Foo").withAttributes("A" := 1).appendAttr("A" := 2),
      expected = XmlNode("Foo").withAttributes("A" := 2)
    )
  }

  test("XmlNode.appendAttr") {
    assertEquals(
      obtained = XmlNode("Foo").appendAttr("A" := 1),
      expected = XmlNode("Foo").withAttributes("A" := 1)
    )
  }

  test("XmlNode.appendAttrs varargs") {
    assertEquals(
      obtained = XmlNode("Foo").appendAttrs(
        "A" := 1,
        "B" := 2
      ),
      expected = XmlNode("Foo").withAttributes(
        "A" := 1,
        "B" := 2
      )
    )
  }

  test("XmlNode.appendAttrs") {
    assertEquals(
      obtained = XmlNode("Foo").appendAttrs(
        Seq(
          "A" := 1,
          "B" := 2
        )
      ),
      expected = XmlNode("Foo").withAttributes(
        Seq(
          "A" := 1,
          "B" := 2
        )
      )
    )
  }

  // ----------- prependAttrs -----------
  test("XmlNode.prependAttr preserve normalization") {
    assertEquals(
      obtained = XmlNode("Foo").withAttributes("A" := 2).prependAttrs("A" := 1),
      expected = XmlNode("Foo").withAttributes("A" := 2)
    )
  }

  test("XmlNode.prependAttr") {
    assertEquals(
      obtained = XmlNode("Foo").prependAttr("A" := 1),
      expected = XmlNode("Foo").withAttributes("A" := 1)
    )
  }

  test("XmlNode.prependAttrs varargs") {
    assertEquals(
      obtained = XmlNode("Foo")
        .withAttributes(
          "C" := 3
        )
        .prependAttrs(
          "A" := 1,
          "B" := 2
        ),
      expected = XmlNode("Foo").withAttributes(
        "A" := 1,
        "B" := 2,
        "C" := 3
      )
    )
  }

  test("XmlNode.prependAttrs") {
    assertEquals(
      obtained = XmlNode("Foo")
        .withAttributes(
          "C" := 3
        )
        .prependAttrs(
          Seq(
            "A" := 1,
            "B" := 2
          )
        ),
      expected = XmlNode("Foo").withAttributes(
        Seq(
          "A" := 1,
          "B" := 2,
          "C" := 3
        )
      )
    )
  }

  // ----------- removeAttr -----------
  test("XmlNode.removeAttr") {
    assertEquals(
      obtained = XmlNode("Foo")
        .withAttributes("A" := 1, "B" := 2)
        .removeAttr("A"),
      expected = XmlNode("Foo").withAttributes("B" := 2)
    )
  }

  // ################### CONTENT ###################
  test("XmlNode.withText") {
    assertEquals(
      obtained = XmlNode("Foo").withText("Text"),
      expected = XmlNode("Foo").withText("Text")
    )
    assertEquals(
      obtained = XmlNode("Foo").withText(""),
      expected = XmlNode("Foo")
    )
  }

  test("XmlNode.textString") {
    assertEquals(
      obtained = XmlNode("Foo").withText("Text").textString,
      expected = "Text"
    )
  }

  test("XmlNode.updateText[T]") {
    assertEquals(
      obtained = XmlNode("Foo")
        .withText("Text")
        .updateText[String](_ ++ "Bar")
        .textString,
      expected = "TextBar"
    )
  }

  test("XmlNode.updateText[T1, T2]") {
    assertEquals(
      XmlNode("Foo").withText(1).updateText[Int, String](_.toString ++ "Bar").textString,
      "1Bar"
    )
  }

  test("XmlNode.updateTextRaw[T]") {
    assertEquals(
      XmlNode("Foo").withText(1).updateTextRaw[String](r => r.asString ++ "Bar").textString,
      "1Bar"
    )

    assertEquals(
      XmlNode("Foo")
        .withChild(XmlNode("Foo"))
        .updateTextRaw[String](_ => "Bar")
        .textString,
      ""
    )

  }
}
