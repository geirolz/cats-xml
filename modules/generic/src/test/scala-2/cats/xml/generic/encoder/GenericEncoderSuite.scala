package cats.xml.generic.encoder

import cats.xml.XmlNode
import cats.xml.codec.Encoder

case class ValueClass(value: String) extends AnyVal
case class Bar(field1: String, field2: BigDecimal)
case class Foo(
  primitiveField: Double,
  valueClass: ValueClass,
  bar: Bar,
  missingField: Option[String],
  missingNode: Option[Bar]
)

class GenericEncoderSuite extends munit.FunSuite {

  import cats.xml.syntax.*

  test("auto") {

    import cats.xml.generic.encoder.auto.*

    implicit val encoderBar: Encoder[Bar] = deriveEncoder[Bar]
    implicit val encoderFoo: Encoder[Foo] = deriveEncoder[Foo]

    assertEquals(
      obtained = Foo(
        primitiveField = 1d,
        valueClass     = ValueClass("TEST"),
        bar            = Bar("BHO", BigDecimal(100)),
        missingField   = None,
        missingNode    = None
      ).toXml,
      expected = XmlNode("Foo")
        .withAttributes(
          "primitiveField" := 1d,
          "valueClass"     := "TEST"
        )
        .withChild(
          XmlNode("Bar")
            .withAttributes(
              "field1" := "BHO",
              "field2" := BigDecimal(100)
            )
        )
    )
  }
}
