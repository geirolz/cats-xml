package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object semiauto {
  private final val defaultDecoder = new MagnoliaDecoder(Configuration.default)

  inline def deriveDecoder[T](using Mirror.Of[T]): Decoder[T] = defaultDecoder.derived[T]
  inline def deriveDecoder[T <: AnyVal & Product: XmlTypeInterpreter]: Decoder[T] =
    defaultDecoder.derived[T]

}
