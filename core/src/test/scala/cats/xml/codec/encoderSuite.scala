package cats.xml.codec

import cats.Eq
import cats.laws.discipline.ContravariantTests
import cats.xml.{XmlData, XmlNode}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbInt

class EncoderSuite extends munit.FunSuite {

  test("Encoder.of") {
    assertEquals(
      Encoder.of[Int](x => XmlNode(x.toString)).encode(1),
      XmlNode("1")
    )
  }

  test("Encoder.pure") {
    assertEquals(
      Encoder.pure(XmlNode("Foo")).encode(1),
      XmlNode("Foo")
    )
  }

  test("Encoder.id") {
    assertEquals(
      Encoder.id.encode(XmlNode("Foo")),
      XmlNode("Foo")
    )
  }

  test("Encoder.apply") {
    assertEquals(
      Encoder[Int].encode(1),
      XmlData.fromInt(1)
    )
  }

}

class EncoderInstancesSuite extends munit.DisciplineSuite {

  import cats.syntax.all.*
  import cats.xml.testing.codec.arbitrary.*

  implicit def eqEncoder[T: Arbitrary: Eq]: Eq[Encoder[T]] =
    (x: Encoder[T], y: Encoder[T]) => {
      val value: T = implicitly[Arbitrary[T]].arbitrary.sample.get
      x.encode(value).eqv(y.encode(value))
    }

  checkAll(
    "Encoder.ContravariantTests",
    ContravariantTests[Encoder].contravariant[Int, Int, String]
  )
}
