package cats.xml.generic.decoder.configured

import cats.data.Validated.Valid
import cats.xml.XmlNode
import cats.xml.codec.Decoder
import cats.xml.generic.Configuration

class ConfiguredDecoderSuite extends munit.FunSuite {

  import cats.xml.syntax.*
  import cats.xml.generic.testing.Samples.*

  // --------------------- AUTO ---------------------
  test("auto configured with useDefaults") {

    case class Foo(
      a: String,
      b: String = "DEFAULT"
    )

    import cats.xml.generic.decoder.configured.auto.*
    implicit val config: Configuration = Configuration.default.withDefaults

    assertEquals(
      obtained = XmlNode("Foo")
        .withAttributes(
          "a" := "TEST"
        )
        .as[Foo],
      expected = Valid(Foo("TEST", "DEFAULT"))
    )
  }

  test("auto configured with ADT with discriminator attr") {

    import cats.xml.generic.decoder.configured.auto.*

    implicit val config: Configuration = Configuration.default
      .withDiscriminatorAttrKey("kind")

    assertEquals(
      obtained = XmlNode("MyVehicle")
        .withAttributes(
          "kind"       := "Bike",
          "wheelCount" := 2
        )
        .as[Vehicle],
      expected = Valid(Bike(2))
    )
  }

  // --------------------- SEMIAUTO ---------------------
  test("semiauto configured with useDefaults") {

    case class Foo(
      a: String,
      b: String = "DEFAULT"
    )

    import cats.xml.generic.decoder.configured.semiauto.*
    implicit val config: Configuration    = Configuration.default.withDefaults
    implicit val decoderFoo: Decoder[Foo] = deriveConfiguredDecoder[Foo]

    assertEquals(
      obtained = XmlNode("Foo")
        .withAttributes(
          "a" := "TEST"
        )
        .as[Foo],
      expected = Valid(Foo("TEST", "DEFAULT"))
    )
  }

  test("semiauto configured with ADT with discriminator attr") {

    import cats.xml.generic.decoder.configured.semiauto.*

    implicit val config: Configuration = Configuration.default
      .withDiscriminatorAttrKey("kind")

    implicit val decoder: Decoder[Vehicle] = deriveConfiguredDecoder[Vehicle]

    assertEquals(
      obtained = XmlNode("MyVehicle")
        .withAttributes(
          "kind"       := "Bike",
          "wheelCount" := 2
        )
        .as[Vehicle],
      expected = Valid(Bike(2))
    )
  }
}
