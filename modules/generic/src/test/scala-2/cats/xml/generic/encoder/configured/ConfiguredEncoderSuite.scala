package cats.xml.generic.encoder.configured

import cats.xml.XmlNode
import cats.xml.codec.Encoder
import cats.xml.generic.Configuration

class ConfiguredEncoderSuite extends munit.FunSuite {

  import cats.xml.generic.testing.Samples.*
  import cats.xml.syntax.*

  // --------------------- AUTO ---------------------
  test("auto configured with ADT with discriminator attr") {

    import cats.xml.generic.encoder.configured.auto.*

    implicit val config: Configuration = Configuration.default
      .withDiscriminatorAttrKey("kind")

    assertEquals(
      obtained = Bike(2).toXmlWiden[Vehicle],
      expected = XmlNode("Vehicle")
        .withAttributes(
          "kind"       := "Bike",
          "wheelCount" := 2
        )
    )
  }

  // --------------------- SEMIAUTO ---------------------
  test("semiauto configured with ADT with discriminator attr") {

    import cats.xml.generic.encoder.configured.semiauto.*

    implicit val config: Configuration = Configuration.default
      .withDiscriminatorAttrKey("kind")

    implicit val encoderVehicle: Encoder[Vehicle] =
      deriveConfiguredEncoder[Vehicle]

    assertEquals(
      obtained = Bike(2).toXmlWiden[Vehicle],
      expected = XmlNode("Vehicle")
        .withAttributes(
          "kind"       := "Bike",
          "wheelCount" := 2
        )
    )
  }
}
