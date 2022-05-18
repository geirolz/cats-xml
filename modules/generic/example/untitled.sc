import cats.xml.codec.{Decoder, Encoder}
import cats.xml.generic.{XmlElemType, XmlTypeInterpreter}
import cats.xml.generic.decoder.auto._
import cats.xml.generic.encoder.semiauto._
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

implicit val ii: XmlTypeInterpreter[Bar] =
  XmlTypeInterpreter
    .withoutText[Bar]
    .overrideType {
      case "wow" => XmlElemType.Text
    }

implicit val decBar: Decoder[Bar] = deriveDecoder[Bar]
val decFoo: Decoder[Foo]          = deriveDecoder[Foo]
//val encFoo: Encoder[Foo]          = deriveEncoder[Foo]


//val a: CursorResultInterpreter[Option[Int]] = CursorResultInterpreterMacro.deriveCursorResultInterpreter[Option[Int]]
//a.interpret(Left(CursorFailure.Custom("")))
//

val barNode = XmlNode("bar").withText(100)
val fooNode =   XmlNode("Foo")
  .withAttributes("test" := "TEST")
  .withChild(barNode)

decFoo.decode(fooNode)
//  .map(encFoo.encode)

//encFoo.encode(Foo(None, "TEST", None, Bar("100")))
