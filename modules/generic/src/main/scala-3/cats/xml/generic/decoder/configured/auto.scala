package cats.xml.generic.decoder.configured

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object auto {

  inline given deriveConfiguredDecoder[T: Mirror.Of: XmlTypeInterpreter](using
    c: Configuration
  ): Decoder[T] = new MagnoliaDecoder(c).autoDerived[T]
  inline given deriveConfiguredDecoder[T <: AnyVal & Product: XmlTypeInterpreter](using
    c: Configuration
  ): Decoder[T] = new MagnoliaDecoder(c).autoDerived[T]

}
