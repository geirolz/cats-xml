package cats.xml.generic.decoder.configured

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object semiauto {

  inline def deriveConfiguredDecoder[T: Mirror.Of](using c: Configuration): Decoder[T] =
    new MagnoliaDecoder(c).autoDerived[T]
  inline def deriveConfiguredDecoder[T <: AnyVal & Product: XmlTypeInterpreter](using
    c: Configuration
  ): Decoder[T] = new MagnoliaDecoder(c).autoDerived[T]

}
