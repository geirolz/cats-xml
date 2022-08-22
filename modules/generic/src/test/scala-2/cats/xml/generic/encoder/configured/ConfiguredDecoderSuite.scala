package cats.xml.generic.encoder.configured

import cats.data.Validated.Valid
import cats.xml.XmlNode
import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, XmlElemType, XmlTypeInterpreter}

class ConfiguredDecoderSuite extends munit.FunSuite {

  import cats.xml.generic.testing.Samples.*
  import cats.xml.syntax.*

  test("configured.auto") {

    import cats.xml.generic.decoder.configured.auto.*

    implicit val typeInterpreterFoo: XmlTypeInterpreter[Foo] =
      XmlTypeInterpreter
        .default[Foo]
        .overrideType(
          _.param(_.valueClass) -> XmlElemType.Attribute
        )

    implicit val config: Configuration    = Configuration.default.withDefaults
    implicit val decoderBar: Decoder[Bar] = deriveConfiguredDecoder[Bar]
    implicit val decoderFoo: Decoder[Foo] = deriveConfiguredDecoder[Foo]

    assertEquals(
      obtained = XmlNode("foo")
        .withAttributes(
          "valueClass" := "TEST"
        )
        .withChild(
          XmlNode("bar")
            .withAttributes(
              "field1" := "BHO",
              "field2" := BigDecimal(100)
            )
        )
        .as[Foo],
      expected = Valid(
        Foo(
          primitiveField = 666d, // default value
          valueClass     = ValueClass("TEST"),
          bar            = Bar("BHO", BigDecimal(100)),
          missingField   = None,
          missingNode    = None
        )
      )
    )
  }

  test("configured.semiauto") {

    import cats.xml.generic.decoder.configured.semiauto.*

    implicit val typeInterpreterFoo: XmlTypeInterpreter[Foo] =
      XmlTypeInterpreter
        .default[Foo]
        .overrideType(
          _.param(_.valueClass) -> XmlElemType.Attribute
        )

    implicit val config: Configuration                  = Configuration.default.withDefaults
    implicit val decoderValueClass: Decoder[ValueClass] = deriveConfiguredDecoder[ValueClass]
    implicit val decoderBar: Decoder[Bar]               = deriveConfiguredDecoder[Bar]
    implicit val decoderFoo: Decoder[Foo]               = deriveConfiguredDecoder[Foo]

    assertEquals(
      obtained = XmlNode("foo")
        .withAttributes(
          "valueClass" := "TEST"
        )
        .withChild(
          XmlNode("bar")
            .withAttributes(
              "field1" := "BHO",
              "field2" := BigDecimal(100)
            )
        )
        .as[Foo],
      expected = Valid(
        Foo(
          primitiveField = 666d, // default value
          valueClass     = ValueClass("TEST"),
          bar            = Bar("BHO", BigDecimal(100)),
          missingField   = None,
          missingNode    = None
        )
      )
    )

  }

}
