package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object semiauto {
  private final val foo = new MagnoliaEncoder(Configuration.default)

  inline def deriveEncoder[T: XmlTypeInterpreter](using Mirror.Of[T]): Encoder[T] = foo.derived[T]
  inline def deriveEncoder[T <: AnyVal & Product: XmlTypeInterpreter]: Encoder[T] = foo.derived[T]
}
