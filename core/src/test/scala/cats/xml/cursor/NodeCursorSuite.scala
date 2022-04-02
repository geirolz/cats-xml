package cats.xml.cursor

import cats.xml.cursor.NodeCursor.Root
import cats.xml.XmlNode
import cats.xml.validator.Validator

class NodeCursorSuite extends munit.FunSuite {

  test("Validate cursor") {
    val cursor = Root.bar.test
      .as[Int]
      .validate(Validator.range(10, 100).and(Validator.min(2)))

    Console.println(
      cursor.focus(
        XmlNode("foo").withChild(XmlNode("bar").withChild(XmlNode("test").withText("1")))
      )
    )
  }
}
