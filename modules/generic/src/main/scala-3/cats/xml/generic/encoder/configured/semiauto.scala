package cats.xml.generic.encoder.configured

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object semiauto {

  inline def deriveConfiguredEncoder[T: Mirror.Of: XmlTypeInterpreter](using
    c: Configuration
  ): Encoder[T] = new MagnoliaEncoder(c).autoDerived[T]
  inline def deriveConfiguredEncoder[T <: AnyVal & Product: XmlTypeInterpreter](using
    c: Configuration
  ): Encoder[T] = new MagnoliaEncoder(c).autoDerived[T]
}
