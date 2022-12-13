package cats.xml.cursor

import cats.xml.{Xml, XmlAttribute, XmlNode}
import cats.xml.cursor.NodeCursor.Root

class NodeCursorSuite extends munit.FunSuite {

  import cats.xml.implicits.*

  test("NodeCursor.selectDynamic") {

    val node: XmlNode =
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
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
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
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

  test("NodeCursor.down with group") {

    val node: XmlNode = {

      XmlNode("Root").withChildren(
        XmlNode("Foo").withChildren(XmlNode("Value").withText(1)),
        XmlNode("Foo").withChildren(XmlNode("Value").withText(2))
      )
    }

    assertEquals(
      obtained = Root.down("Foo").down("Value").focus(node),
      expected = Right(
        XmlNode.group(
          XmlNode("Value").withText(1),
          XmlNode("Value").withText(2)
        )
      )
    )
  }

  test("NodeCursor.downWildcard") {

    val node: XmlNode =
      XmlNode("wrapper").withChildren(
        XmlNode("root").withChildren(
          XmlNode("foo"),
          XmlNode("bar"),
          XmlNode("baz")
        )
      )

    assertEquals(
      obtained = Root.down("root").downWildcard.focus(node),
      expected = Right(
        XmlNode.group(
          XmlNode("foo"),
          XmlNode("bar"),
          XmlNode("baz")
        )
      )
    )

    assertEquals(
      obtained = Root.down("root").*.focus(node),
      expected = Right(
        XmlNode.group(
          XmlNode("foo"),
          XmlNode("bar"),
          XmlNode("baz")
        )
      )
    )
  }

  test("NodeCursor.\\") {

    val node: XmlNode =
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
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
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
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
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
          XmlNode("bar").withAttributes("attr" := 1),
          XmlNode("bar").withAttributes("attr" := 2),
          XmlNode("bar").withAttributes("attr" := 3)
        )
      )

    assertEquals(
      obtained = (Root \\ "bar").filter(_.existsAttrWithValue[Int]("attr", _ % 2 != 0)).focus(node),
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
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
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
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
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
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
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
      XmlNode("root").withChildren(
        XmlNode("bar").withAttributes("attr" := 1),
        XmlNode("bar").withAttributes("attr" := 2),
        XmlNode("bar").withAttributes("attr" := 3)
      )

    assertEquals(
      obtained = Root
        .down("bar")
        .filter(_.findAttr[Int]("attr").contains(1))
        .focus(node),
      expected = Right(
        XmlNode("bar").withAttributes("attr" := 1)
      )
    )

    assertEquals(
      obtained = Root.down("bar").filter(_.findAttr[Int]("attr").contains(100)).focus(node),
      expected = Right(XmlNode.emptyGroup)
    )
  }

  test("NodeCursor.|") {

    val node: XmlNode =
      XmlNode("root").withChildren(
        XmlNode("bar").withAttributes("attr" := 1),
        XmlNode("bar").withAttributes("attr" := 2),
        XmlNode("bar").withAttributes("attr" := 3)
      )

    def withAttrEqTo(value: Int): XmlNode => Boolean =
      _.findAttr[Int]("attr").contains(value)

    assertEquals(
      obtained = (Root.down("bar") | withAttrEqTo(1)).focus(node),
      expected = Right(
        XmlNode("bar").withAttributes("attr" := 1)
      )
    )

    assertEquals(
      obtained = (Root.down("bar") | withAttrEqTo(100)).focus(node),
      expected = Right(XmlNode.emptyGroup)
    )
  }

  test("NodeCursor.find") {

    val node: XmlNode =
      XmlNode("root").withChildren(
        XmlNode("foo").withChildren(
          XmlNode("bar0").withAttributes("a" := "1"),
          XmlNode("bar1").withAttributes("a" := "2"),
          XmlNode("bar2").withAttributes("a" := "3")
        )
      )

    assertEquals(
      obtained = Root.foo.find(_.findAttrRaw("a").exists(_.value.asString == "3")).focus(node),
      expected = Right(XmlNode("bar2").withAttributes("a" := "3"))
    )
  }

  test("NodeCursor.head") {

    assertEquals(
      obtained = Root.foo.head.focus(
        XmlNode("root").withChildren(
          XmlNode("foo").withChildren(
            XmlNode("bar0"),
            XmlNode("bar1"),
            XmlNode("bar2")
          )
        )
      ),
      expected = Right(XmlNode("bar0"))
    )

    assertEquals(
      obtained = Root.foo.head.focus(XmlNode("root").withChildren(XmlNode("foo"))),
      expected = Left(CursorFailure.MissingNodeHead("/foo"))
    )
  }

  test("NodeCursor.last") {

    assertEquals(
      obtained = Root.foo.last.focus(
        XmlNode("root").withChildren(
          XmlNode("foo").withChildren(
            XmlNode("bar0"),
            XmlNode("bar1"),
            XmlNode("bar2")
          )
        )
      ),
      expected = Right(XmlNode("bar2"))
    )

    assertEquals(
      obtained = Root.foo.last.focus(XmlNode("root").withChildren(XmlNode("foo"))),
      expected = Left(CursorFailure.MissingNodeLast("/foo"))
    )
  }

  test("NodeCursor.attr") {

    val node: XmlNode =
      XmlNode("root").withChildren(
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
      XmlNode("root").withChildren(
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
      XmlNode("root").withChildren(
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
      XmlNode("root").withChildren(
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
      XmlNode("root").withChildren(
        XmlNode("foo").withText("TEST")
      )

    assertEquals(
      obtained = Root.foo.text.focus(node),
      expected = Right(Xml.Data.fromString("TEST"))
    )

    assertEquals(
      obtained = Root.missing.text.focus(node),
      expected = Left(CursorFailure.MissingNode("", "missing"))
    )
  }

  test("NodeCursor.history") {
    val predicate: XmlNode => Boolean = _ => true
    assertEquals(
      obtained = Root.noop.foo
        .deepDown("bar")
        .atIndex(1)
        .filter(predicate)
        .find(predicate)
        .head
        .last
        .history,
      expected = List(
        NodeCursor.Op.Down("foo"),
        NodeCursor.Op.DeepDown("bar"),
        NodeCursor.Op.SelectNodeByIndex(1),
        NodeCursor.Op.Filter(predicate),
        NodeCursor.Op.FindChild(predicate),
        NodeCursor.Op.Head,
        NodeCursor.Op.Last
      )
    )
  }

  test("NodeCursor.path") {
    val predicate: XmlNode => Boolean = _ => true
    assertEquals(
      obtained = Root.noop.foo
        .deepDown("bar")
        .atIndex(1)
        .filter(predicate)
        .find(predicate)
        .head
        .last
        .path,
      expected = s"/foo//bar[1][filter $predicate][find $predicate]/head/last"
    )
  }

  test("NodeCursor.equals") {
    val predicate: XmlNode => Boolean = _ => true
    assertEquals(
      obtained = Root.noop.foo
        .deepDown("bar")
        .atIndex(1)
        .filter(predicate)
        .find(predicate)
        .head
        .last,
      expected = Root.noop.foo
        .deepDown("bar")
        .atIndex(1)
        .filter(predicate)
        .find(predicate)
        .head
        .last
    )
  }
}
