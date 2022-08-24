package cats.xml

import cats.xml.codec.{DataEncoder, Decoder}
import cats.xml.testing.XmlValidName
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.reflect.ClassTag

class XmlAttributeSuite extends munit.ScalaCheckSuite {

  import cats.xml.syntax.*

  // isomorphism
  testAttributeDataIso[Unit]
  testAttributeDataIso[Int]
  testAttributeDataIso[Long]
  testAttributeDataIso[Float]
  testAttributeDataIso[Boolean]
  testAttributeDataIso[String]
  testAttributeDataIso[BigDecimal]
  testAttributeDataIso[BigInt]

  // equality
  testAttributeEquality[Unit]
  testAttributeEquality[Int]
  testAttributeEquality[Long]
  testAttributeEquality[Float]
  testAttributeEquality[Boolean]
  testAttributeEquality[String]
  testAttributeEquality[BigDecimal]
  testAttributeEquality[BigInt]

  test("XmlAttribute.normalizeAttrs") {

    assertEquals(
      XmlAttribute.normalizeAttrs(
        List(
          "A" := 1,
          "B" := 2,
          "C" := 3,
          "A" := 4,
          "B" := 5,
          "C" := 6
        )
      ),
      List(
        "A" := 4,
        "B" := 5,
        "C" := 6
      )
    )

    assertEquals(
      XmlAttribute.normalizeAttrs(
        List(
          "first"  := 1,
          "second" := 2,
          "third"  := 3,
          "first"  := 4,
          "second" := 5,
          "third"  := 6
        )
      ),
      List(
        "first"  := 4,
        "second" := 5,
        "third"  := 6
      )
    )
  }

  private def testAttributeDataIso[T: Arbitrary: DataEncoder: Decoder](implicit
    c: ClassTag[T]
  ): Unit =
    property(s"XmlAttribute create an attr with ${c.runtimeClass.getSimpleName} data") {
      forAll { (key: XmlValidName, value: T) =>
        assertEquals(
          obtained = XmlAttribute(key.value, value).value.as[T].toOption,
          expected = Some(value)
        )
      }
    }

  private def testAttributeEquality[T: Arbitrary: DataEncoder: Decoder](implicit
    c: ClassTag[T]
  ): Unit =
    property(
      s"Two XmlAttribute with the same (key, ${c.runtimeClass.getSimpleName}) are equals"
    ) {
      forAll { (key: XmlValidName, value: T) =>
        assertEquals(
          obtained = XmlAttribute(key.value, value),
          expected = XmlAttribute(key.value, value)
        )
      }
    }
}
