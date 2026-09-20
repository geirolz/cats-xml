package cats.xml.generic.encoder.configured

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object auto {

  inline given deriveConfiguredEncoder[T: Mirror.Of: XmlTypeInterpreter](using
    c: Configuration
  ): Encoder[T] = new MagnoliaEncoder(c).autoDerived[T]
  inline given deriveConfiguredEncoder[T <: AnyVal & Product: XmlTypeInterpreter](using
    c: Configuration
  ): Encoder[T] = new MagnoliaEncoder(c).autoDerived[T]
}
