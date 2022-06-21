package cats.xml.cursor

import cats.xml.{XmlAttribute, XmlNode}
import cats.xml.cursor.NodeCursor.Root
import cats.xml.XmlData.XmlString

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

  test("NodeCursor deepDown \\\\") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar").withAttributes("attr" := 1),
          XmlNode("bar").withAttributes("attr" := 2),
          XmlNode("bar").withAttributes("attr" := 3)
        )
      )

    assertEquals(
      obtained = (Root \\ "bar").focus(node),
      expected = Right(
        XmlNode.group(
          XmlNode("bar").withAttributes("attr" := 1),
          XmlNode("bar").withAttributes("attr" := 2),
          XmlNode("bar").withAttributes("attr" := 3)
        )
      )
    )

    assertEquals(
      obtained = (Root \\ "missing").focus(node),
      expected = Right(XmlNode.emptyGroup)
    )
  }

  test("NodeCursor deepDown and filter") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar").withAttributes("attr" := 1),
          XmlNode("bar").withAttributes("attr" := 2),
          XmlNode("bar").withAttributes("attr" := 3)
        )
      )

    assertEquals(
      obtained = (Root \\ "bar").filter(_.existsAttrValue[Int]("attr", _ % 2 != 0)).focus(node),
      expected = Right(
        XmlNode.group(
          XmlNode("bar").withAttributes("attr" := 1),
          XmlNode("bar").withAttributes("attr" := 3)
        )
      )
    )

    assertEquals(
      obtained = (Root \\ "missing").focus(node),
      expected = Right(XmlNode.emptyGroup)
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

  test("NodeCursor.applyDynamic") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar0"),
          XmlNode("bar1"),
          XmlNode("bar2")
        )
      )

    assertEquals(
      obtained = Root.foo(2).focus(node),
      expected = Right(XmlNode("bar2"))
    )

    assertEquals(
      obtained = Root.foo(10).focus(node),
      expected = Left(CursorFailure.MissingNodeAtIndex("/foo", 10))
    )
  }

  test("NodeCursor.apply with index") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar0"),
          XmlNode("bar1"),
          XmlNode("bar2")
        )
      )

    assertEquals(
      obtained = Root.foo.apply(2).focus(node),
      expected = Right(XmlNode("bar2"))
    )

    assertEquals(
      obtained = Root.foo.apply(10).focus(node),
      expected = Left(CursorFailure.MissingNodeAtIndex("/foo", 10))
    )
  }

  test("NodeCursor.filter") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("bar").withAttributes("attr" := 1),
        XmlNode("bar").withAttributes("attr" := 2),
        XmlNode("bar").withAttributes("attr" := 3)
      )

    assertEquals(
      obtained = Root
        .filter(_.findAttrValue[Int]("attr").contains(1))
        .focus(node),
      expected = Right(
        XmlNode.group(
          XmlNode("bar").withAttributes("attr" := 1)
        )
      )
    )

    assertEquals(
      obtained = Root.filter(_.findAttrValue[Int]("attr").contains(100)).focus(node),
      expected = Right(XmlNode.emptyGroup)
    )
  }

  test("NodeCursor.|") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("bar").withAttributes("attr" := 1),
        XmlNode("bar").withAttributes("attr" := 2),
        XmlNode("bar").withAttributes("attr" := 3)
      )

    def withAttrEqTo(value: Int): XmlNode => Boolean =
      _.findAttrValue[Int]("attr").contains(value)

    assertEquals(
      obtained = (Root | withAttrEqTo(1)).focus(node),
      expected = Right(
        XmlNode.group(
          XmlNode("bar").withAttributes("attr" := 1)
        )
      )
    )

    assertEquals(
      obtained = (Root | withAttrEqTo(100)).focus(node),
      expected = Right(XmlNode.emptyGroup)
    )
  }

  test("NodeCursor.find") {

    val node: XmlNode =
      XmlNode("root").withChild(
        XmlNode("foo").withChild(
          XmlNode("bar0").withAttributes("a" := "1"),
          XmlNode("bar1").withAttributes("a" := "2"),
          XmlNode("bar2").withAttributes("a" := "3")
        )
      )

    assertEquals(
      obtained = Root.foo.find(_.findAttr("a").exists(_.value.asString == "3")).focus(node),
      expected = Right(XmlNode("bar2").withAttributes("a" := "3"))
    )
  }

  test("NodeCursor.head") {

    assertEquals(
      obtained = Root.foo.head.focus(
        XmlNode("root").withChild(
          XmlNode("foo").withChild(
            XmlNode("bar0"),
            XmlNode("bar1"),
            XmlNode("bar2")
          )
        )
      ),
      expected = Right(XmlNode("bar0"))
    )

    assertEquals(
      obtained = Root.foo.head.focus(XmlNode("root").withChild(XmlNode("foo"))),
      expected = Left(CursorFailure.MissingNodeHead("/foo"))
    )
  }

  test("NodeCursor.last") {

    assertEquals(
      obtained = Root.foo.last.focus(
        XmlNode("root").withChild(
          XmlNode("foo").withChild(
            XmlNode("bar0"),
            XmlNode("bar1"),
            XmlNode("bar2")
          )
        )
      ),
      expected = Right(XmlNode("bar2"))
    )

    assertEquals(
      obtained = Root.foo.last.focus(XmlNode("root").withChild(XmlNode("foo"))),
      expected = Left(CursorFailure.MissingNodeLast("/foo"))
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
