package cats.xml.codec

import cats.data.NonEmptyList
import cats.laws.discipline.MonadErrorTests
import cats.xml.{Xml, XmlNode}
import cats.xml.testing.Samples.dummyNode
import cats.Eq
import cats.data.Validated.Valid

import scala.util.{Failure, Success}

class DecoderSuite extends munit.ScalaCheckSuite {

  import cats.implicits.*

  test("Decoder.map") {
    assertEquals(
      obtained = Decoder.id
        .map {
          case node: XmlNode => node.text.map(_.toString)
          case _             => None
        }
        .decode(XmlNode("Root").withText("Text")),
      expected = "Text".some.validNel
    )
  }

  test("Decoder.emap - Right with DecodingFailure") {
    assertEquals(
      obtained = Decoder.id
        .emap(_ => 1.asRight)
        .decode(dummyNode),
      expected = 1.validNel
    )
  }

  test("Decoder.emap - Left with DecodingFailure") {
    assertEquals(
      obtained = Decoder.id
        .emap(_ => DecoderFailure.Custom("Missing").asLeft)
        .decode(dummyNode),
      expected = DecoderFailure.Custom("Missing").invalidNel
    )
  }

  test("Decoder.emap - Right with Throwable") {
    assertEquals(
      obtained = Decoder.id
        .emap(_ => 1.asRight)
        .decode(dummyNode),
      expected = 1.validNel
    )
  }

  test("Decoder.emap - Left with Throwable") {
    val ex: RuntimeException = new RuntimeException("Missing")
    assertEquals(
      obtained = Decoder.id
        .emap(_ => ex.asLeft)
        .decode(dummyNode),
      expected = DecoderFailure.Error(ex).invalidNel
    )
  }

  test("Decoder.emapTry - Success") {
    assertEquals(
      obtained = Decoder.id
        .emapTry(_ => Success(1))
        .decode(dummyNode),
      expected = 1.validNel
    )
  }

  test("Decoder.emapTry - Failure") {
    val ex: RuntimeException = new RuntimeException("Missing")
    assertEquals(
      obtained = Decoder.id
        .emapTry(_ => Failure(ex))
        .decode(dummyNode),
      expected = DecoderFailure.Error(ex).invalidNel
    )
  }

  test("Decoder.flatMapF - success >=> success") {
    assertEquals(
      obtained = Decoder.id
        .flatMapF(_ => 1.validNel)
        .decode(dummyNode),
      expected = 1.validNel
    )
  }

  test("Decoder.flatMapF - success >=> failed") {
    assertEquals(
      obtained = Decoder.id
        .flatMapF(_ => DecoderFailure.Custom("ERROR").invalidNel)
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR").invalidNel
    )
  }

  test("Decoder.flatMapF - failed >=> success") {
    assertEquals(
      obtained = Decoder
        .const[Int](
          DecoderFailure.Custom("ERROR").invalidNel
        )
        .flatMapF(_ => 1.validNel)
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR").invalidNel
    )
  }

  test("Decoder.flatMapF - failed >=> failed") {
    assertEquals(
      obtained = Decoder
        .const[Int](
          DecoderFailure.Custom("ERROR 1").invalidNel
        )
        .flatMapF(_ => DecoderFailure.Custom("ERROR 2").invalidNel)
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR 1").invalidNel
    )
  }

  test("Decoder.flatMap - success >=> success") {
    assertEquals(
      obtained = Decoder.id
        .flatMap(_ =>
          Decoder.const(
            Valid(1)
          )
        )
        .decode(dummyNode),
      expected = Valid(1)
    )
  }

  test("Decoder.flatMap - success >=> failed") {
    assertEquals(
      obtained = Decoder.id
        .flatMap(_ =>
          Decoder.const[Xml](
            DecoderFailure.Custom("ERROR").invalidNel
          )
        )
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR").invalidNel
    )
  }

  test("Decoder.flatMap - failed >=> success") {
    assertEquals(
      obtained = Decoder
        .const[Int](
          DecoderFailure.Custom("ERROR").invalidNel
        )
        .flatMap(_ =>
          Decoder.const[Int](
            1.validNel
          )
        )
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR").invalidNel
    )
  }

  test("Decoder.flatMap - failed >=> failed") {
    assertEquals(
      obtained = Decoder
        .const[Int](
          DecoderFailure.Custom("ERROR 1").invalidNel
        )
        .flatMap(_ =>
          Decoder.const[Int](
            DecoderFailure.Custom("ERROR 2").invalidNel
          )
        )
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR 1").invalidNel
    )
  }
}

class DecoderInstancesSuite extends munit.DisciplineSuite {

  import cats.implicits.*
  import cats.laws.discipline.arbitrary.*
  import cats.xml.testing.codec.arbitrary.*

  // TODO TO FIX
  implicit def eqDecoder[T]: Eq[Decoder[T]] = Eq.allEqual

  checkAll(
    "Decoder.MonadErrorLaws",
    MonadErrorTests[Decoder, NonEmptyList[DecoderFailure]]
      .monadError[Int, Int, String]
  )
}

class DecoderCompanionSuite extends munit.ScalaCheckSuite {

  import cats.implicits.*
  import cats.xml.implicits.*

  test("Decoder.id") {
    val node: XmlNode = dummyNode
    assertEquals(
      obtained = Decoder.id.decode(node),
      expected = node.validNel
    )
  }

  test("Decoder.apply summoner") {
    implicit val decoder: Decoder[Xml] = Decoder.id
    assertEquals(
      obtained = Decoder[Xml],
      expected = decoder
    )
  }

  test("Decoder.pure") {
    assertEquals(
      obtained = Decoder.pure(1).decode(dummyNode),
      expected = 1.validNel
    )
  }

  test("Decoder.const with success") {
    assertEquals(
      obtained = Decoder
        .const(1.validNel)
        .decode(dummyNode),
      expected = 1.validNel
    )
  }

  test("Decoder.failed") {
    assertEquals(
      obtained = Decoder
        .failure(DecoderFailure.Custom("ERROR"))
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR").invalidNel
    )
  }

  test("Decoder.const with failed") {
    assertEquals(
      obtained = Decoder
        .const(DecoderFailure.Custom("ERROR").invalidNel)
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR").invalidNel
    )
  }

  test("Decoder.fromCursor") {

    case class Foo(intAttr: Int, boolAttr: Boolean, bar: Bar)
    case class Bar(text: BigDecimal)

    val decoder: Decoder[Foo] = Decoder
      .fromCursor(c => {
        for {
          intAttr  <- c.attr("intAttr").as[Int]
          boolAttr <- c.attr("boolAttr").as[Boolean]
          bar      <- c.down("Bar").text.as[BigDecimal].map(Bar.apply)
        } yield Foo(intAttr, boolAttr, bar)
      })

    assertEquals(
      obtained = decoder.decode(
        XmlNode("Foo")
          .withAttributes(
            "intAttr"  := 10,
            "boolAttr" := true
          )
          .withChild(
            XmlNode("Bar").withText(BigDecimal(100))
          )
      ),
      expected = Foo(
        intAttr  = 10,
        boolAttr = true,
        bar      = Bar(BigDecimal(100))
      ).validNel
    )
  }

  test("Decoder.fromEither - Right") {
    assertEquals(
      obtained = Decoder
        .fromEither(_ => 1.asRight)
        .decode(dummyNode),
      expected = 1.validNel
    )
  }

  test("Decoder.fromEither - Left DecodingFailure") {
    assertEquals(
      obtained = Decoder
        .fromEither(_ => DecoderFailure.Custom("ERROR").asLeft)
        .decode(dummyNode),
      expected = DecoderFailure.Custom("ERROR").invalidNel
    )
  }

  test("Decoder.fromEither - Left Throwable") {

    val ex: RuntimeException = new RuntimeException("ERROR")
    assertEquals(
      obtained = Decoder
        .fromEither(_ => ex.asLeft)
        .decode(dummyNode),
      expected = DecoderFailure.Error(ex).invalidNel
    )
  }

  test("Decoder.fromTry - Success") {

    val ex: RuntimeException = new RuntimeException("ERROR")
    assertEquals(
      obtained = Decoder
        .fromTry(_ => Failure(ex))
        .decode(dummyNode),
      expected = DecoderFailure.Error(ex).invalidNel
    )
  }

  test("Decoder.fromTry - Failure") {
    assertEquals(
      obtained = Decoder
        .fromTry(_ => Success(1))
        .decode(dummyNode),
      expected = 1.validNel
    )
  }
}
