package cats.xml.codec

import cats.xml.XmlNode

class CodecSuite extends munit.FunSuite {

  import cats.implicits.*
  import cats.xml.implicits.*

  case class Foo(bar: String, baz: Int)

  test("Codec is isomorphic") {

    val codec: Codec[Foo] = Codec.of(
      decoder = Decoder.fromCursor(c =>
        (
          c.attr("bar").as[String],
          c.attr("baz").as[Int]
        ).mapN(Foo)
      ),
      encoder = Encoder.of(foo => {
        XmlNode("Foo")
          .withAttributes(
            "bar" := foo.bar,
            "baz" := foo.baz
          )
      })
    )

    assertEquals(
      obtained = codec.decode(codec.encode(Foo("test", 100))),
      expected = Foo("test", 100).validNel
    )
  }

  test("Codec to Decoder") {

    implicit val codec: Codec[Foo] = Codec.of(
      decoder = Decoder.fromCursor(c =>
        (
          c.attr("bar").as[String],
          c.attr("baz").as[Int]
        ).mapN(Foo)
      ),
      encoder = Encoder.of(_ => XmlNode(""))
    )

    assertEquals(
      obtained = Decoder[Foo].decode(
        XmlNode("Foo")
          .withAttributes(
            "bar" := "test",
            "baz" := 100
          )
      ),
      expected = Foo("test", 100).validNel
    )
  }

  test("Codec to Encoder") {

    implicit val codec: Codec[Foo] = Codec.of(
      decoder = Decoder.failure(DecoderFailure.Custom("Not implemented.")),
      encoder = Encoder.of(foo => {
        XmlNode("Foo")
          .withAttributes(
            "bar" := foo.bar,
            "baz" := foo.baz
          )
      })
    )

    assertEquals(
      obtained = Encoder[Foo].encode(
        Foo("test", 100)
      ),
      expected = XmlNode("Foo")
        .withAttributes(
          "bar" := "test",
          "baz" := 100
        )
    )
  }
}
