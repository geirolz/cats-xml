package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}

import scala.deriving.Mirror

object auto {
  private final val defaultEncoder = new MagnoliaEncoder(Configuration.default)

  inline given deriveEncoder[T: XmlTypeInterpreter](using Mirror.Of[T]): Encoder[T] =
    defaultEncoder.autoDerived[T]
  inline given deriveEncoder[T <: AnyVal & Product: XmlTypeInterpreter]: Encoder[T] =
    defaultEncoder.autoDerived[T]

}
