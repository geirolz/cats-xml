package cats.xml

import cats.xml.testing.VeryLongNumericString
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.reflect.ClassTag

class XmlDataSuite extends munit.ScalaCheckSuite {

  // testDataEquality
  testDataEquality[String]
  testDataEquality[Char]
  testDataEquality[Boolean]
  testDataEquality[Int]
  testDataEquality[Short]
  testDataEquality[Long]
  testDataEquality[Float]
  testDataEquality[Double]
  testDataEquality[BigInt]
  testDataEquality[BigDecimal]
  testDataEquality[VeryLongNumericString]
  testDataEquality[Seq[String]]
  testDataEquality[Array[String]]

  private def testDataEquality[T: Arbitrary](implicit
    c: ClassTag[T]
  ): Unit =
    property(
      s"XmlData eq works comparing actual string representation for ${c.runtimeClass.getSimpleName}"
    ) {
      forAll { (value: T) =>
        assertEquals(
          obtained = Xml.string(value.toString).widen,
          expected = Xml.data(value)
        )
      }
    }
}
