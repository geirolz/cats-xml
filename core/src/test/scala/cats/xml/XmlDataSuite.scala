package cats.xml

import cats.xml.testing.VeryLongNumericString
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.reflect.ClassTag

class XmlDataSuite extends munit.ScalaCheckSuite {

  testFromDataString[String]
  testFromDataString[Char]
  testFromDataString[Boolean]
  testFromDataString[Int]
  testFromDataString[Short]
  testFromDataString[Long]
  testFromDataString[Float]
  testFromDataString[Double]
  testFromDataString[BigInt]
  testFromDataString[BigDecimal]
  testFromDataString[VeryLongNumericString]

  private def testFromDataString[T: Arbitrary](implicit
    c: ClassTag[T]
  ): Unit =
    property(s"Xml.fromDataString works with ${c.runtimeClass.getSimpleName}") {
      forAll { (value: T) =>
        assertEquals(
          obtained = Xml.fromDataString(value.toString).toString,
          expected = value.toString
        )
      }
    }
}
