package cats.xml.generic.decoder

import cats.data.Validated.Valid
import cats.xml.XmlNode
import cats.xml.codec.Decoder
import cats.xml.codec.DecoderFailure.CursorFailed
import cats.xml.cursor.CursorFailure
import cats.xml.generic.testing.Samples

class DecoderSuite extends munit.FunSuite {

  import cats.syntax.all.*
  import cats.xml.syntax.*
  import Samples.*

  // --------------------- AUTO ---------------------
  test("auto") {

    import cats.xml.generic.decoder.auto.*

    assertEquals(
      obtained = XmlNode("foo")
        .withAttributes(
          "primitiveField" := 1d,
          "valueClass"     := "TEST"
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
          primitiveField = 1d,
          valueClass     = ValueClass("TEST"),
          bar            = Bar("BHO", BigDecimal(100)),
          missingField   = None,
          missingNode    = None
        )
      )
    )
  }

  test("auto without useDefaults") {

    import cats.xml.generic.decoder.auto.*

    case class Foo(
      a: String,
      b: String = "DEFAULT"
    )

    assertEquals(
      obtained = XmlNode("Foo")
        .withAttributes(
          "a" := "TEST"
        )
        .as[Foo],
      expected = CursorFailed(
        CursorFailure.MissingAttrByKey(
          path = "/@b",
          key  = "b"
        )
      ).invalidNel
    )
  }

  test("auto with ADT") {

    import cats.xml.generic.decoder.auto.*

    assertEquals(
      obtained = XmlNode("Bike")
        .withAttributes(
          "wheelCount" := 2
        )
        .as[Vehicle],
      expected = Valid(Bike(2))
    )
  }

  // --------------------- SEMIAUTO ---------------------
  test("semiauto") {

    import cats.xml.generic.decoder.semiauto.*

    implicit val decoderValueClass: Decoder[ValueClass] = deriveDecoder[ValueClass]
    implicit val decoderBar: Decoder[Bar]               = deriveDecoder[Bar]
    implicit val decoderFoo: Decoder[Foo]               = deriveDecoder[Foo]

    assertEquals(
      obtained = XmlNode("foo")
        .withAttributes(
          "primitiveField" := 1d,
          "valueClass"     := "TEST"
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
          primitiveField = 1d,
          valueClass     = ValueClass("TEST"),
          bar            = Bar("BHO", BigDecimal(100)),
          missingField   = None,
          missingNode    = None
        )
      )
    )

  }

  test("semiauto with ADT") {

    import cats.xml.generic.decoder.semiauto.*

    implicit val decoder: Decoder[Vehicle] = deriveDecoder[Vehicle]

    assertEquals(
      obtained = XmlNode("Bike")
        .withAttributes(
          "wheelCount" := 2
        )
        .as[Vehicle],
      expected = Valid(Bike(2))
    )
  }
}
