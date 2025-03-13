package cats.xml.generic.decoder.configured

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object auto {

  inline given deriveConfiguredDecoder[T](using m: Mirror.Of[T], c: Configuration): Decoder[T] =
    new MagnoliaDecoder(c).autoDerived[T]
  inline given deriveEncoder[T <: AnyVal & Product: XmlTypeInterpreter](using
    c: Configuration
  ): Decoder[T] =
    new MagnoliaDecoder(c).autoDerived[T]

}
