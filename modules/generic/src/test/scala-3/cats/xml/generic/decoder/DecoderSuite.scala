package cats.xml.generic.decoder

import cats.data.Validated.Valid
import cats.xml.XmlNode
import cats.xml.codec.Decoder
import cats.xml.generic.{Samples, XmlElemType, XmlTypeInterpreter}

class DecoderSuite extends munit.FunSuite {

  import cats.xml.syntax.given
  import Samples.*

  test("auto") {

    given XmlTypeInterpreter[Foo] =
      XmlTypeInterpreter
        .default[Foo]
        .overrideType(
          _.param(_.valueClass) -> XmlElemType.Attribute
        )

    assertEquals(
      obtained = XmlNode("foo")
        .withAttrs(
          "primitiveField" := 1d,
          "valueClass"     := "TEST"
        )
        .withChildren(
          XmlNode("bar")
            .withAttrs(
              "field1" := "BHO",
              "field2" := BigDecimal(100)
            ),
          XmlNode("valueClass2").withAttrs("v" := "OK!")
        )
        .as[Foo],
      expected = Valid(
        Foo(
          primitiveField = 1d,
          valueClass     = ValueClass("TEST"),
          valueClass2    = ValueClass2(InsideValueClass("OK!")),
          bar            = Bar("BHO", BigDecimal(100)),
          missingField   = None,
          missingNode    = None
        )
      )
    )
  }
}
