package cats.xml.effect

import cats.effect.IO
import cats.xml.{Xml, XmlNode}

class XmlEffectLoaderSuite extends munit.CatsEffectSuite {

  import cats.xml.implicits.*
  import cats.xml.effect.implicits.*

  test("XmlLoader.fromString") {
    assertIO(
      Xml.fromString[IO]("""
          |<Foo intAttr="1" boolAttr="true">
          |    <Bar intAttr="2" emptyAttr="">
          |        <Baz>100</Baz>
          |    </Bar>
          |</Foo>""".stripMargin),
      XmlNode("Foo")
        .withAttributes(
          "intAttr"  := 1,
          "boolAttr" := true
        )
        .withChild(
          XmlNode("Bar")
            .withAttributes(
              "intAttr"   := 2,
              "emptyAttr" := ""
            )
            .withChild(
              XmlNode("Baz").withText(100)
            )
        )
    )
  }

  test("XmlLoader.loadResourceFile") {
    assertIO(
      Xml.loadResourceFile[IO]("/simple_document_1.xml").use(IO(_)),
      XmlNode("Foo")
        .withAttributes(
          "intAttr"  := 1,
          "boolAttr" := true
        )
        .withChild(
          XmlNode("Bar")
            .withAttributes(
              "intAttr"   := 2,
              "emptyAttr" := ""
            )
            .withChild(
              XmlNode("Baz").withText(100)
            )
        )
    )
  }
}
