package cats.xml

import cats.xml.codec.{DataEncoder, Decoder}
import cats.xml.testing.XmlValidName
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.reflect.ClassTag

class XmlAttributeSuite extends munit.ScalaCheckSuite {

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
