package cats.xml.modifier

import cats.data.Validated.Valid
import cats.xml.XmlNode
import cats.xml.syntax.XmlAttrStringOps

class ModifierSuite extends munit.FunSuite {

  test("Modifier works as expected") {
    val node: XmlNode =
      XmlNode("wrapper")
        .withAttrs("a" := "1", "b" := "2")
        .withChildren(
          XmlNode("root")
            .withAttrs("a" := "1", "b" := "2")
            .withChildren(
              XmlNode("foo")
                .withChildren(
                  XmlNode("baz")
                    .withAttrs("a" := "1", "b" := "2")
                    .withChildren(
                      XmlNode("bar")
                        .withChildren(
                          XmlNode("value")
                            .withText(1)
                        )
                    )
                )
            )
        )

    val result: Modifier.Result[XmlNode] =
      node.modify(_.root.foo.baz.bar.value.modifyNode(_.withText(2)))

    assertEquals(
      obtained = node.focus(_.root.foo.baz.bar.value.text.as[Int]),
      expected = Valid(1)
    )

    assertEquals(
      obtained = result.map(_.focus(_.root.foo.baz.bar.value.text.as[Int])),
      expected = Right(Valid(2))
    )
  }

  test("Modifier.id") {
    assertEquals(
      obtained = Modifier.id[String]("FOO"),
      expected = Right("FOO")
    )
  }

  test("Modifier.const") {
    assertEquals(
      obtained = Modifier.const[String](Right("FOO"))("BAR"),
      expected = Right("FOO")
    )
  }

  test("Modifier.failed") {
    assertEquals(
      obtained = Modifier.failed[String](ModifierFailure.Custom("BOOM!"))("BAR"),
      expected = Left(ModifierFailure.Custom("BOOM!"))
    )
  }
}
