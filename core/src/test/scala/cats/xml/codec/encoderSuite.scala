package cats.xml.codec

import cats.Eq
import cats.laws.discipline.ContravariantTests
import cats.xml.{Xml, XmlData, XmlNode}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Prop.forAll

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
      Xml.ofInt(1)
    )
  }

}

class EncoderInstancesSuite extends munit.ScalaCheckSuite {

  import cats.xml.testing.arbitrary.CommonArbitrary.*
  import cats.xml.testing.arbitrary.XmlDataArbitrary.*

  property(s"Encoder.encodeOption") {
    forAll { (value: Option[Int]) =>
      assertEquals(
        obtained = Encoder[Option[Int]].encode(value),
        expected = value.fold[XmlData](Xml.Null)(Xml.ofInt)
      )
    }
  }

  property(s"Encoder.encodeOption - Some") {
    forAll { (value: Some[Int]) =>
      assertEquals(
        obtained = Encoder[Some[Int]].encode(value),
        expected = Xml.ofInt(value.value)
      )
    }
  }

  test("DataEncoder.encodeOption - None") {
    assertEquals(
      obtained = Encoder[None.type].encode(None),
      expected = Xml.Null
    )
  }

  property(s"Encoder.dataEncodeOption") {
    forAll { (value: Option[Int]) =>
      assertEquals(
        obtained = Encoder[Option[Int]].encode(value),
        expected = value.fold[XmlData](Xml.Null)(Xml.ofInt)
      )
    }
  }

  property(s"Encoder.dataEncodeOption - Some") {
    forAll { (value: Some[Int]) =>
      assertEquals(
        obtained = Encoder[Some[Int]].encode(value),
        expected = Xml.ofInt(value.value)
      )
    }
  }

  test("DataEncoder.dataEncodeOption - None") {
    assertEquals(
      obtained = Encoder[None.type].encode(None),
      expected = Xml.Null
    )
  }

  test("Encoder.encodeUnit") {
    assertEquals(
      obtained = Encoder[Unit].encode(()),
      expected = Xml.Null
    )
  }

  property(s"Encoder.encoderXmlData") {
    forAll { (value: XmlData) =>
      assertEquals(
        obtained = Encoder[XmlData].encode(value),
        expected = value
      )
    }
  }

  property(s"Encoder.encodeString") {
    forAll { (value: String) =>
      assertEquals(
        obtained = Encoder[String].encode(value),
        expected = Xml.ofString(value)
      )
    }
  }

  property(s"Encoder.encodeChar") {
    forAll { (value: Char) =>
      assertEquals(
        obtained = Encoder[Char].encode(value),
        expected = Xml.ofChar(value)
      )
    }
  }

  property(s"Encoder.encodeBoolean") {
    forAll { (value: Boolean) =>
      assertEquals(
        obtained = Encoder[Boolean].encode(value),
        expected = Xml.ofBoolean(value)
      )
    }
  }

  property(s"Encoder.encodeByte") {
    forAll { (value: Byte) =>
      assertEquals(
        obtained = Encoder[Byte].encode(value),
        expected = Xml.ofByte(value)
      )
    }
  }

  property(s"Encoder.encodeShort") {
    forAll { (value: Short) =>
      assertEquals(
        obtained = Encoder[Short].encode(value),
        expected = Xml.ofShort(value)
      )
    }
  }

  property(s"Encoder.encodeInt") {
    forAll { (value: Int) =>
      assertEquals(
        obtained = Encoder[Int].encode(value),
        expected = Xml.ofInt(value)
      )
    }
  }

  property(s"Encoder.encodeLong") {
    forAll { (value: Long) =>
      assertEquals(
        obtained = Encoder[Long].encode(value),
        expected = Xml.ofLong(value)
      )
    }
  }

  property(s"Encoder.encodeFloat") {
    forAll { (value: Float) =>
      assertEquals(
        obtained = Encoder[Float].encode(value),
        expected = Xml.ofFloat(value)
      )
    }
  }

  property(s"Encoder.encodeDouble") {
    forAll { (value: Double) =>
      assertEquals(
        obtained = Encoder[Double].encode(value),
        expected = Xml.ofDouble(value)
      )
    }
  }

  property(s"Encoder.encodeBigDecimal") {
    forAll { (value: BigDecimal) =>
      assertEquals(
        obtained = Encoder[BigDecimal].encode(value),
        expected = Xml.ofBigDecimal(value)
      )
    }
  }

  property(s"Encoder.encodeBigInt") {
    forAll { (value: BigInt) =>
      assertEquals(
        obtained = Encoder[BigInt].encode(value),
        expected = Xml.ofBigInt(value)
      )
    }
  }
}

class EncoderCatsInstancesSuite extends munit.DisciplineSuite {

  import cats.syntax.all.*
  import cats.xml.testing.arbitrary.CodecArbitrary.*

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
