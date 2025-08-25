package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object auto {
  private final val defaultDecoder = new MagnoliaDecoder(Configuration.default)

  inline given deriveDecoder[T: Mirror.Of: XmlTypeInterpreter]: Decoder[T] =
    defaultDecoder.autoDerived[T]
  inline given deriveEncoder[T <: AnyVal & Product: XmlTypeInterpreter]: Decoder[T] =
    defaultDecoder.autoDerived[T]

}
