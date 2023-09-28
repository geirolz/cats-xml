package cats.xml

import cats.xml.syntax.XmlAttrStringOps
import org.scalacheck.Prop.forAll

class xmlSuite extends munit.ScalaCheckSuite {

  import cats.xml.testing.arbitrary.XmlArbitrary.*

  property(s"Xml.duplicate works as expected") {
    forAll { (value: Xml) =>
      assertEquals(
        obtained = value.duplicate,
        expected = value
      )
    }
  }

  test("Xml.duplicate works with XmlNode") {

    val node: XmlNode = XmlNode("Foo").withChildren(
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

    assertEquals(
      obtained = node.duplicate,
      expected = node
    )
  }
}
