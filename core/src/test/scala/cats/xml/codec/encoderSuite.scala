package cats.xml.codec

import cats.Eq
import cats.laws.discipline.ContravariantTests
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbInt

class EncoderSuite extends munit.FunSuite {

  test("") {}
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
