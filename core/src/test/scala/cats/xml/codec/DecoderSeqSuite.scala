package cats.xml.codec

import cats.data.Validated.Valid
import cats.implicits.{catsSyntaxOptionId, catsSyntaxTuple2Semigroupal}
import cats.xml.XmlNode
import cats.xml.syntax.DecoderOps

// TODO: Move to decoder suite
class DecoderSeqSuite extends munit.FunSuite {

  case class Baz(v: String)
  object Baz {
    implicit val bazD: Decoder[Baz] =
      Decoder.fromCursor(_.text.as[String]).map(Baz(_))
  }

  case class Qux(v: String)
  object Qux {
    implicit val quxD: Decoder[Qux] =
      Decoder.fromCursor(_.text.as[String]).map(Qux(_))

  }

  case class Bar(baz: Baz, qux: Option[Qux])
  object Bar {
    implicit val barD: Decoder[Bar] =
      Decoder.fromCursor { c =>
        (
          c.down("baz").as[Baz],
          c.down("qux").as[Option[Qux]]
        ).mapN(Bar.apply)
      }
  }

  case class Foo(bar: List[Bar])
  object Foo {
    implicit val fooD: Decoder[Foo] =
      Decoder.fromCursor { c =>
        c.down("bar")
          .as[List[Bar]]
          .map(Foo.apply)
      }
  }

  test("Decoder[Seq[A]] with XmlNode") {

    val data: XmlNode =
      XmlNode("foo")
        .withChildren(
          XmlNode("bar")
            .withChildren(
              XmlNode("baz").withText("1")
            ),
          XmlNode("bar")
            .withChildren(
              XmlNode("baz").withText("2")
            ),
          XmlNode("bar")
            .withChildren(
              XmlNode("baz").withText("3"),
              XmlNode("qux").withText("A")
            )
        )

    assertEquals(
      data.as[Foo],
      Valid(
        Foo(
          List[Bar](
            Bar(Baz("1"), None),
            Bar(Baz("2"), None),
            Bar(Baz("3"), Qux("A").some)
          )
        )
      )
    )
  }

  test("Decoder[Seq[A]] with XmlGroup") {

    val data: XmlNode =
      XmlNode("foo")
        .withChildren(
          XmlNode.group(
            XmlNode("bar")
              .withChildren(
                XmlNode("baz").withText("1")
              ),
            XmlNode("bar")
              .withChildren(
                XmlNode("baz").withText("2")
              ),
            XmlNode("bar")
              .withChildren(
                XmlNode("baz").withText("3"),
                XmlNode("qux").withText("A")
              )
          )
        )

    assertEquals(
      data.as[Foo],
      Valid(
        Foo(
          List[Bar](
            Bar(Baz("1"), None),
            Bar(Baz("2"), None),
            Bar(Baz("3"), Qux("A").some)
          )
        )
      )
    )
  }
}
