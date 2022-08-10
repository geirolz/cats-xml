import cats.xml.utils.generic.TypeInfo
import cats.xml.codec.{Decoder, Encoder}
import cats.xml.generic.{XmlElemType, XmlTypeInterpreter}
import cats.xml.generic.decoder.auto._
import cats.xml.generic.encoder.auto._
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

implicit val typeInfoBar: TypeInfo[Bar] = TypeInfo.deriveTypeInfo[Bar]

implicit val ii: XmlTypeInterpreter[Bar] =
  XmlTypeInterpreter
    .withoutText[Bar]
    .overrideType(
      _.param(_.wow) -> XmlElemType.Text
    )

implicit val decBar: Decoder[Bar] = deriveDecoder[Bar]
val decFoo: Decoder[Foo]          = deriveDecoder[Foo]

implicit val encBar: Encoder[Bar] = deriveEncoder[Bar]
val encFoo: Encoder[Foo]          = deriveEncoder[Foo]

val barNode = XmlNode("bar").withText(100)
val fooNode = XmlNode("Foo")
  .withAttributes("test" := "TEST")
  .withChild(barNode)

val decoderResult: Decoder.Result[Foo] = decFoo.decode(fooNode)
val encoderResult                      = encFoo.encode(decFoo.decode(fooNode).toOption.get)
//  .map(encFoo.encode)

//encFoo.encode(Foo(None, "TEST", None, Bar("100")))
//TypeInfo.deriveTypeInfo[String]
//TypeInfo.deriveTypeInfo[Int]




