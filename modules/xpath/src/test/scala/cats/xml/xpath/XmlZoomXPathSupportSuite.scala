package cats.xml.xpath

import cats.xml.cursor.NodeCursor.Root
import cats.xml.XmlNode
import cats.xml.syntax.XmlAttrStringOps
import cats.xml.xpath.error.XPathError.NotSupportedConstruction
import eu.cdevreeze.xpathparser.ast.{AttributeAxisAbbrevForwardStep, SimpleNameTest}
import eu.cdevreeze.xpathparser.ast.EQName.QName
import eu.cdevreeze.xpathparser.common.UnprefixedName

class XmlZoomXPathSupportSuite extends munit.FunSuite {

  import cats.xml.xpath.implicits.*

  test("Absolute path") {
    assertEquals(
      obtained = xpath"/root",
      expected = Right(Root.down("root"))
    )
  }

  test("Absolute path with selector") {
    assertEquals(
      obtained = xpath"/root/@id",
      expected = Left(
        NotSupportedConstruction(
          feature = AttributeAxisAbbrevForwardStep(
            nodeTest = SimpleNameTest(
              name = QName(
                qname = UnprefixedName(
                  localPart = "id"
                )
              )
            )
          )
        )
      )
    )
  }

  test("Absolute path with index") {
    assertEquals(
      obtained = xpath"/root[1]",
      expected = Right(Root.down("root").atIndex(1))
    )
  }

  test("Absolute path with attribute selector") {

    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root").withText("FOO1"),
          XmlNode("root")
            .withAttrs(
              "id" := 1
            )
        )

    assertEquals(
      obtained = xpath"/root[@id='1']".toTryValue.get.focus(data),
      expected = Right(
        XmlNode("root").withAttrs(
          "id" := 1
        )
      )
    )
  }

  test("Absolute path with text()") {
    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withAttrs(
              "id" := 1
            )
            .withText("pew"),
          XmlNode("root")
            .withAttrs(
              "id" := 2
            )
            .withText("mew")
        )

    assertEquals(
      obtained = xpath"/root[text()='mew']".toTryValue.get.focus(data),
      expected = Right(
        XmlNode("root")
          .withAttrs(
            "id" := 2
          )
          .withText("mew")
      )
    )
  }

  test("Absolute path with child predicate (>)") {

    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value>4]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode("value").withText(5)
      )
    )
  }

  test("Absolute path with child predicate (>=)") {
    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value>=4]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode.group(
          XmlNode("value").withText(4),
          XmlNode("value").withText(5)
        )
      )
    )
  }

  test("Absolute path with child predicate (=)") {
    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value=4]/value".toTryValue.get.focus(data),
      expected = Right(XmlNode("value").withText(4))
    )
  }

  test("Absolute path with child predicate (!=)") {
    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value!=4]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode.group(
          XmlNode("value").withText(3),
          XmlNode("value").withText(5)
        )
      )
    )
  }

  test("Absolute path with child predicate (<)") {
    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value<4]/value".toTryValue.get.focus(data),
      expected = Right(XmlNode("value").withText(3))
    )
  }

  test("Absolute path with child predicate (<=)") {
    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value<=4]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode.group(
          XmlNode("value").withText(3),
          XmlNode("value").withText(4)
        )
      )
    )
  }

  test("Absolute path with child predicate (text() =)") {
    val data: XmlNode.Node =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value/text()='4']/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode("value").withText(4)
      )
    )
  }

  test("Absolute path with child predicate (contains(text()))") {

    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText("foo")),
              XmlNode("child").withChildren(XmlNode("value").withText("bar"))
            )
        )
    assertEquals(
      obtained = xpath"/root/child[value/contains(text(),'ar')]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode("value").withText("bar")
      )
    )
  }

  test("Absolute path with child predicate (starts-with(text()))") {

    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText("foo")),
              XmlNode("child").withChildren(XmlNode("value").withText("bar"))
            )
        )
    assertEquals(
      obtained =
        xpath"/root/child[value/starts-with(text(),'ba')]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode("value").withText("bar")
      )
    )
  }

  test("Absolute path with child predicate (ends-with(text()))") {

    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText("foo")),
              XmlNode("child").withChildren(XmlNode("value").withText("bar"))
            )
        )
    assertEquals(
      obtained = xpath"/root/child[value/ends-with(text(),'o')]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode("value").withText("foo")
      )
    )
  }

  test("Path with wildcard") {
    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )
    assertEquals(
      obtained = xpath"/root/*/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode.group(
          XmlNode("value").withText(3),
          XmlNode("value").withText(4),
          XmlNode("value").withText(5)
        )
      )
    )
  }

  test("Last child") {
    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )
    assertEquals(
      obtained = xpath"/root/child[last()]/value".toTryValue.get.focus(data),
      expected = Right(XmlNode("value").withText(5))
    )
  }

  test("Or logic") {
    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child")
                .withAttrs("id" := 1)
                .withChildren(XmlNode("value").withText(3)),
              XmlNode("child")
                .withAttrs("id" := 2)
                .withChildren(XmlNode("value").withText(4)),
              XmlNode("child")
                .withAttrs("id" := 3)
                .withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[@id='1' or @id='2']/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode.group(
          XmlNode("value").withText(3),
          XmlNode("value").withText(4)
        )
      )
    )
  }

  test("And logic") {
    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child").withChildren(XmlNode("value").withText(3)),
              XmlNode("child").withChildren(XmlNode("value").withText(4)),
              XmlNode("child").withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[value>3 and value<5]/value".toTryValue.get.focus(data),
      expected = Right(XmlNode("value").withText(4))
    )
  }

  test("Attribute contains") {
    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child")
                .withAttrs("id" := "foo")
                .withChildren(XmlNode("value").withText(3)),
              XmlNode("child")
                .withAttrs("id" := "bar")
                .withChildren(XmlNode("value").withText(4)),
              XmlNode("child")
                .withAttrs("id" := "baz")
                .withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[contains(@id, 'ba')]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode.group(
          XmlNode("value").withText(4),
          XmlNode("value").withText(5)
        )
      )
    )
  }

  test("Attribute starts-with") {
    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child")
                .withAttrs("id" := "foo")
                .withChildren(XmlNode("value").withText(3)),
              XmlNode("child")
                .withAttrs("id" := "bar")
                .withChildren(XmlNode("value").withText(4)),
              XmlNode("child")
                .withAttrs("id" := "baz")
                .withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[starts-with(@id, 'ba')]/value".toTryValue.get.focus(data),
      expected = Right(
        XmlNode.group(
          XmlNode("value").withText(4),
          XmlNode("value").withText(5)
        )
      )
    )
  }

  test("Attribute ends-with") {

    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child")
                .withAttrs("id" := "foo")
                .withChildren(XmlNode("value").withText(3)),
              XmlNode("child")
                .withAttrs("id" := "bar")
                .withChildren(XmlNode("value").withText(4)),
              XmlNode("child")
                .withAttrs("id" := "baz")
                .withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[ends-with(@id, 'az')]/value".toTryValue.get.focus(data),
      expected = Right(XmlNode("value").withText(5))
    )
  }

  test("Attribute not contains") {
    val data =
      XmlNode("wrapper")
        .withChildren(
          XmlNode("root")
            .withChildren(
              XmlNode("child")
                .withAttrs("id" := "foo")
                .withChildren(XmlNode("value").withText(3)),
              XmlNode("child")
                .withAttrs("id" := "bar")
                .withChildren(XmlNode("value").withText(4)),
              XmlNode("child")
                .withAttrs("id" := "baz")
                .withChildren(XmlNode("value").withText(5))
            )
        )

    assertEquals(
      obtained = xpath"/root/child[not(contains(@id, 'ba'))]/value".toTryValue.get.focus(data),
      expected = Right(XmlNode("value").withText(3))
    )
  }

  test("Union") {
    assertEquals(
      obtained = xpath"/root/child[@id='1']/value | /root/child[@id='2']/value".isLeft,
      expected = true
    )
  }

  test("Traversal path") {
    assertEquals(
      obtained = xpath"//root".isLeft,
      expected = true
    )
  }
}
