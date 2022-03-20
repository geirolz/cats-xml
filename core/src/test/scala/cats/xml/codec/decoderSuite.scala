package cats.xml.codec

import cats.xml.{Xml, XmlNode}
import cats.xml.Samples.dummyNode

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
      expected = Decoder.Result.success("Text".some)
    )
  }

  test("Decoder.emap - Right with DecodingFailure") {
    assertEquals(
      obtained = Decoder.id
        .emap(_ => 1.asRight)
        .decode(dummyNode),
      expected = Decoder.Result.success(1)
    )
  }

  test("Decoder.emap - Left with DecodingFailure") {
    assertEquals(
      obtained = Decoder.id
        .emap(_ => DecodingFailure.custom("Missing").asLeft)
        .decode(dummyNode),
      expected = Decoder.Result.failed(DecodingFailure.custom("Missing"))
    )
  }

  test("Decoder.emap - Right with Throwable") {
    assertEquals(
      obtained = Decoder.id
        .emap(_ => 1.asRight)
        .decode(dummyNode),
      expected = Decoder.Result.success(1)
    )
  }

  test("Decoder.emap - Left with Throwable") {
    val ex: RuntimeException = new RuntimeException("Missing")
    assertEquals(
      obtained = Decoder.id
        .emap(_ => ex.asLeft)
        .decode(dummyNode),
      expected = Decoder.Result.failed(DecodingFailure.error(ex))
    )
  }

  test("Decoder.emapTry - Success") {
    assertEquals(
      obtained = Decoder.id
        .emapTry(_ => Success(1))
        .decode(dummyNode),
      expected = Decoder.Result.success(1)
    )
  }

  test("Decoder.emapTry - Failure") {
    val ex: RuntimeException = new RuntimeException("Missing")
    assertEquals(
      obtained = Decoder.id
        .emapTry(_ => Failure(ex))
        .decode(dummyNode),
      expected = Decoder.Result.failed(DecodingFailure.error(ex))
    )
  }
}

class DecoderCompanionSuite extends munit.ScalaCheckSuite {

  import cats.implicits.*
  import cats.xml.implicits.*

  test("Decoder.id") {
    val node: XmlNode = dummyNode
    assertEquals(
      obtained = Decoder.id.decode(node),
      expected = Decoder.Result.success(node)
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
      expected = Decoder.Result.success(1)
    )
  }

  test("Decoder.const with success") {
    assertEquals(
      obtained = Decoder
        .const(Decoder.Result.success(1))
        .decode(dummyNode),
      expected = Decoder.Result.success(1)
    )
  }

  test("Decoder.failed") {
    assertEquals(
      obtained = Decoder
        .failed(DecodingFailure.custom("ERROR"))
        .decode(dummyNode),
      expected = Decoder.Result.failed(
        DecodingFailure.custom("ERROR")
      )
    )
  }

  test("Decoder.const with failed") {
    assertEquals(
      obtained = Decoder
        .const(Decoder.Result.failed(DecodingFailure.custom("ERROR")))
        .decode(dummyNode),
      expected = Decoder.Result.failed(
        DecodingFailure.custom("ERROR")
      )
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
      expected = Decoder.Result.success(
        Foo(
          intAttr  = 10,
          boolAttr = true,
          bar      = Bar(BigDecimal(100))
        )
      )
    )
  }

  test("Decoder.fromEither - Right") {
    assertEquals(
      obtained = Decoder
        .fromEither(_ => 1.asRight)
        .decode(dummyNode),
      expected = Decoder.Result.success(1)
    )
  }

  test("Decoder.fromEither - Left DecodingFailure") {
    assertEquals(
      obtained = Decoder
        .fromEither(_ => DecodingFailure.custom("ERROR").asLeft)
        .decode(dummyNode),
      expected = Decoder.Result.failed(
        DecodingFailure.custom("ERROR")
      )
    )
  }

  test("Decoder.fromEither - Left Throwable") {

    val ex: RuntimeException = new RuntimeException("ERROR")
    assertEquals(
      obtained = Decoder
        .fromEither(_ => ex.asLeft)
        .decode(dummyNode),
      expected = Decoder.Result.failed(
        DecodingFailure.error(ex)
      )
    )
  }

  test("Decoder.fromTry - Success") {

    val ex: RuntimeException = new RuntimeException("ERROR")
    assertEquals(
      obtained = Decoder
        .fromTry(_ => Failure(ex))
        .decode(dummyNode),
      expected = Decoder.Result.failed(
        DecodingFailure.error(ex)
      )
    )
  }

  test("Decoder.fromTry - Failure") {
    assertEquals(
      obtained = Decoder
        .fromTry(_ => Success(1))
        .decode(dummyNode),
      expected = Decoder.Result.success(1)
    )
  }
}
