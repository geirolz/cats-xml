package cats.xml.cursor

import cats.xml.{XmlAttribute, XmlNode, XmlString}
import cats.xml.cursor.NodeCursor.Root

class NodeCursorSuite extends munit.FunSuite {

  import cats.xml.implicits.*

  test("NodeCursor.selectDynamic") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar")
        )
      )

    assertEquals(
      obtained = Root.foo.bar.focus(node),
      expected = Right(XmlNode("bar"))
    )

    assertEquals(
      obtained = Root.foo.missing.focus(node),
      expected = Left(CursorFailure.MissingNode("/foo", "missing"))
    )
  }

  test("NodeCursor.down") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar")
        )
      )

    assertEquals(
      obtained = Root.down("foo").down("bar").focus(node),
      expected = Right(XmlNode("bar"))
    )

    assertEquals(
      obtained = Root.down("foo").down("missing").focus(node),
      expected = Left(CursorFailure.MissingNode("/foo", "missing"))
    )
  }

  test("NodeCursor.\\") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar")
        )
      )

    assertEquals(
      obtained = (Root \ "foo" \ "bar").focus(node),
      expected = Right(XmlNode("bar"))
    )

    assertEquals(
      obtained = (Root \ "foo" \ "missing").focus(node),
      expected = Left(CursorFailure.MissingNode("/foo", "missing"))
    )
  }

  test("NodeCursor.downPath") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar")
        )
      )

    assertEquals(
      obtained = Root.downPath("foo/bar").focus(node),
      expected = Right(XmlNode("bar"))
    )

    assertEquals(
      obtained = Root.downPath("foo/missing").focus(node),
      expected = Left(CursorFailure.MissingNode("/foo", "missing"))
    )
  }

  test("NodeCursor.attr") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withAttributes("bar" := "TEST")
      )

    assertEquals(
      obtained = Root.foo.attr("bar").focus(node),
      expected = Right(XmlAttribute("bar", "TEST"))
    )

    assertEquals(
      obtained = Root.foo.attr("missing").focus(node),
      expected = Left(CursorFailure.MissingAttrByKey("/foo/@missing", "missing"))
    )
  }

  test("NodeCursor.attrAt") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withAttributes("bar" := "TEST")
      )

    assertEquals(
      obtained = Root.foo.attrAt(0).focus(node),
      expected = Right(XmlAttribute("bar", "TEST"))
    )

    assertEquals(
      obtained = Root.foo.attrAt(5).focus(node),
      expected = Left(CursorFailure.MissingAttrAtIndex("/foo/@[5]", 5))
    )
  }

  test("NodeCursor.attrHead") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withAttributes(
          "first"  := "TEST_1",
          "second" := "TEST_2"
        )
      )

    assertEquals(
      obtained = Root.foo.attrHead.focus(node),
      expected = Right(XmlAttribute("first", "TEST_1"))
    )
  }

  test("NodeCursor.attrLast") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withAttributes(
          "first"  := "TEST_1",
          "second" := "TEST_2"
        )
      )

    assertEquals(
      obtained = Root.foo.attrLast.focus(node),
      expected = Right(XmlAttribute("second", "TEST_2"))
    )
  }

  test("NodeCursor.text") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withText("TEST")
      )

    assertEquals(
      obtained = Root.foo.text.focus(node),
      expected = Right(XmlString("TEST"))
    )

    assertEquals(
      obtained = Root.missing.text.focus(node),
      expected = Left(CursorFailure.MissingNode("", "missing"))
    )
  }
}
