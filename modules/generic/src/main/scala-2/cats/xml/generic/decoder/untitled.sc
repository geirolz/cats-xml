import cats.xml.codec.Decoder
import cats.xml.generic.decoder.semiauto.*
import cats.xml.XmlNode
import cats.xml.implicits.*

case class Foo(test: String)

val dec: Decoder[Foo] = deriveDecoder[Foo]

dec.decode(XmlNode("Foo").withAttributes("test" := "TEST"))
