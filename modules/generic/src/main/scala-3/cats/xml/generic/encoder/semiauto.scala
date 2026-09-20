package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object semiauto {
  private final val defaultEncoder = new MagnoliaEncoder(Configuration.default)

  inline def deriveEncoder[T: Mirror.Of: XmlTypeInterpreter]: Encoder[T] =
    defaultEncoder.derived[T]
  inline def deriveEncoder[T <: AnyVal & Product: XmlTypeInterpreter]: Encoder[T] =
    defaultEncoder.derived[T]
}
