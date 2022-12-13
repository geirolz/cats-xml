package cats.xml.effect

import cats.effect.IO
import cats.xml.{Xml, XmlNode}

class XmlLoaderSuite extends munit.CatsEffectSuite {

  import cats.xml.implicits.*
  import cats.xml.effect.implicits.*

  test("XmlLoader.loadResourceFile") {
    assertIO(
      Xml.loadResourceFile[IO]("/simple_document_1.xml").use(IO(_)),
      XmlNode("Foo")
        .withAttributes(
          "intAttr"  := 1,
          "boolAttr" := true
        )
        .withChildren(
          XmlNode("Bar")
            .withAttributes(
              "intAttr"   := 2,
              "emptyAttr" := ""
            )
            .withChildren(
              XmlNode("Baz").withText(100)
            )
        )
    )
  }
}
