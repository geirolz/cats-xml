package cats.xml

import cats.xml.testing.VeryLongNumericString
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.reflect.ClassTag

class XmlDataSuite extends munit.ScalaCheckSuite {

  // fromNumberString
  testFromNumberString[Int]()
  testFromNumberString[Short]()
  testFromNumberString[Long]()
  testFromNumberString[Float]()
  testFromNumberString[Double]()
  testFromNumberString[BigInt]()
  testFromNumberString[BigDecimal]()
  testFromNumberString[String](shouldFail                = true)
  testFromNumberString[VeryLongNumericString](shouldFail = true)

  // fromDataString
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

  private def testFromNumberString[T: Arbitrary](shouldFail: Boolean = false)(implicit
    c: ClassTag[T]
  ): Unit =
    if (shouldFail)
      property(s"Xml.fromNumberString return None with ${c.runtimeClass.getSimpleName}") {
        forAll { (value: T) =>
          assertEquals(
            obtained = Xml.fromNumberString(value.toString),
            expected = None
          )
        }
      }
    else
      property(s"Xml.fromNumberString works with ${c.runtimeClass.getSimpleName}") {
        forAll { (value: T) =>
          assertEquals(
            Xml.fromNumberString(value.toString).get.toBigDecimal,
            Some(BigDecimal(value.toString))
          )
        }
      }

  private def testFromDataString[T: Arbitrary](implicit
    c: ClassTag[T]
  ): Unit =
    property(s"Xml.fromDataString works with ${c.runtimeClass.getSimpleName}") {
      forAll { (value: T) =>
        Xml.fromDataString(value.toString) match {
          case number: XmlData.XmlNumber =>
            assertEquals(
              number.toBigDecimal,
              Some(BigDecimal(value.toString))
            )
          case other =>
            assertEquals(
              obtained = other.toString,
              expected = value.toString
            )
        }

      }
    }
}
