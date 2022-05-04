import cats.xml.codec.Decoder
import cats.xml.generic.decoder.auto._
//import cats.xml.generic.encoder.semiauto._
import cats.xml.XmlNode
import cats.xml.implicits._

case class Stringa(value1: String) extends AnyVal

case class Bar(wow: String)
case class Foo(
  missing: Option[String],
  test: Stringa,
  missingNode: Option[Bar],
  bar: Bar
)


implicit val decBar: Decoder[Bar] = deriveDecoder[Bar]
val decFoo: Decoder[Foo]          = deriveDecoder[Foo]
//val encFoo: Encoder[Foo]          = deriveEncoder[Foo]


//val a: CursorResultInterpreter[Option[Int]] = CursorResultInterpreterMacro.deriveCursorResultInterpreter[Option[Int]]
//a.interpret(Left(CursorFailure.Custom("")))
//

val barNode = XmlNode("bar").withAttributes("wow" := 100)
val fooNode =   XmlNode("Foo")
  .withAttributes("test" := "TEST")
  .withChild(barNode)

decFoo.decode(fooNode)

//encFoo.encode(Foo(None, "TEST", None, Bar("100")))
