package cats.xml.generic.encoder.configured

import cats.xml.XmlNode
import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, XmlTypeInterpreter}
import cats.xml.utils.generic.ParamName

class ConfiguredEncoderSuite extends munit.FunSuite {

  import cats.xml.generic.Samples.*
  import cats.xml.syntax.*

  // --------------------- AUTO ---------------------
  test("auto configured with ADT with discriminator attr") {

    import cats.xml.generic.encoder.configured.auto.{given, *}

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

    import cats.xml.generic.encoder.configured.semiauto.{given, *}

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

    import cats.xml.generic.encoder.configured.semiauto.{given, *}

    implicit val config: Configuration = Configuration.default
      .withUseLabelsForNodes(true)
    implicit val fooXmlTypeInterpreter: XmlTypeInterpreter[Bar] =
      XmlTypeInterpreter.auto[Bar]((_, _) => false, (_, _) => false)
    implicit val insideValueClassXmlTypeInterpreter: XmlTypeInterpreter[InsideValueClass] =
      XmlTypeInterpreter.auto[InsideValueClass]((_, _) => false, (_, _) => false)
    implicit val barXmlTypeInterpreter: XmlTypeInterpreter[Foo] =
      XmlTypeInterpreter.auto[Foo]((_, _) => false, (_, _) => false)

    implicit val encoderValueClass: Encoder[ValueClass] = deriveConfiguredEncoder[ValueClass]
    implicit val encoderInsideValueClass: Encoder[InsideValueClass] =
      deriveConfiguredEncoder[InsideValueClass]
    implicit val encoderValueClass2: Encoder[ValueClass2] = deriveConfiguredEncoder[ValueClass2]
    implicit val encoderBar: Encoder[Bar]                 = deriveConfiguredEncoder[Bar]
    implicit val encoderFoo: Encoder[Foo]                 = deriveConfiguredEncoder[Foo]

    assertEquals(
      obtained = Foo(
        primitiveField = 666d,
        valueClass     = ValueClass("hi"),
        valueClass2    = ValueClass2(InsideValueClass("OK!")),
        bar            = Bar("f1", BigDecimal(12.34)),
        missingField   = None,
        missingNode    = None
      ).toXmlWiden[Foo],
      expected = XmlNode("Foo")
        .withChildren(
          XmlNode("primitiveField").withText(666d),
          XmlNode("valueClass").withText("hi"),
          XmlNode("valueClass2")
            .withChildren(XmlNode("v").withText("OK!")),
          XmlNode("bar")
            .withChildren(
              XmlNode("field1").withText("f1"),
              XmlNode("field2").withText(BigDecimal(12.34))
            )
        )
    )
  }
}
