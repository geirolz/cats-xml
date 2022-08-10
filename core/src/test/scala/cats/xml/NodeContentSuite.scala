package cats.xml

import cats.data.NonEmptyList
import cats.xml.codec.{DataEncoder, Decoder}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.reflect.ClassTag

class NodeContentSuite extends munit.ScalaCheckSuite {

  test("NodeContent.empty is empty") {
    assert(
      NodeContent.empty.isEmpty
    )
  }

  test("NodeContent.text('FOO') is NOT empty") {
    assert(
      NodeContent.text("FOO").nonEmpty
    )
  }

  test("NodeContent.text('') is NOT empty") {
    assert(
      NodeContent.text("").nonEmpty
    )
  }

  test("NodeContent.children with 1 child is NOT empty") {
    assert(
      NodeContent.children(NonEmptyList.of(XmlNode("Foo"))).nonEmpty
    )
  }

  // ------ PROPERTY ------
  testContentTextIso[Int]
  testContentTextIso[Long]
  testContentTextIso[Float]
  testContentTextIso[Boolean]
  testContentTextIso[String]
  testContentTextIso[BigDecimal]
  testContentTextIso[BigInt]

  private def testContentTextIso[T: Arbitrary: DataEncoder: Decoder](implicit
    c: ClassTag[T]
  ): Unit =
    property(s"NodeContent.text create content with ${c.runtimeClass.getSimpleName}") {
      forAll { (value: T) =>
        assertEquals(
          obtained = NodeContent.text(value).text.flatMap(_.as[T].toOption),
          expected = Some(value)
        )
      }
    }
}
