package cats.xml.generic.encoder.configured

import cats.xml.XmlNode
import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, XmlTypeInterpreter}

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
        .withAttrs(
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
        .withAttrs(
          "kind"       := "Bike",
          "wheelCount" := 2
        )
    )
  }

  test("semiauto configured with useLabelsForNodes") {

    import cats.xml.generic.encoder.configured.semiauto.*

    implicit val config: Configuration = Configuration.default
      .withUseLabelsForNodes(true)
    implicit def FooXmlTypeInterpreter: XmlTypeInterpreter[Bar] =
      XmlTypeInterpreter.auto[Bar]((_, _) => false, (_, _) => false)
    implicit lazy val BarXmlTypeInterpreter: XmlTypeInterpreter[Foo] =
      XmlTypeInterpreter.auto[Foo]((_, _) => false, (_, _) => false)

    implicit val encoderBar: Encoder[Bar]               = deriveConfiguredEncoder[Bar]
    implicit val encoderValueClass: Encoder[ValueClass] = deriveConfiguredEncoder[ValueClass]
    implicit val encoderFoo: Encoder[Foo]               = deriveConfiguredEncoder[Foo]

    assertEquals(
      obtained = Foo(
        primitiveField = 666d,
        valueClass     = ValueClass("hi"),
        bar            = Bar("f1", BigDecimal(12.34)),
        missingField   = None,
        missingNode    = None
      ).toXmlWiden[Foo],
      expected = XmlNode("Foo")
        .withChildren(
          XmlNode("primitiveField").withText(666d),
          XmlNode("valueClass").withText("hi"),
          XmlNode("bar")
            .withChildren(
              XmlNode("field1").withText("f1"),
              XmlNode("field2").withText(BigDecimal(12.34))
            )
        )
    )
  }
}
