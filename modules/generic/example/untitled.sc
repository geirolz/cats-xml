import cats.xml.codec.{Decoder, Encoder}
import cats.xml.generic.decoder.semiauto._
//import cats.xml.generic.encoder.semiauto._
import cats.xml.XmlNode
import cats.xml.implicits._

case class Bar(value: String)
case class Foo(
  missing: Option[String],
  test: String,
  missingNode: Option[Bar],
  bar: Bar
)

implicit val decBar: Decoder[Bar] = deriveDecoder[Bar]
val decFoo: Decoder[Foo]          = deriveDecoder[Foo]
//val encFoo: Encoder[Foo]          = deriveEncoder[Foo]

//val a: CursorResultInterpreter[Option[Int]] = CursorResultInterpreterMacro.deriveCursorResultInterpreter[Option[Int]]
//a.interpret(Left(CursorFailure.Custom("")))
//
decFoo.decode(
  XmlNode("Foo")
    .withAttributes("test" := "TEST")
    .withChild(
      XmlNode("bar").withAttributes("value" := 100)
    )
)

//encFoo.encode(Foo(None, "TEST", None, Bar("100")))
