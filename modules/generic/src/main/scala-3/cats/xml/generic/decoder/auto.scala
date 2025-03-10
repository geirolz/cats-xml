package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder}
import scala.deriving.Mirror

object auto {

  inline given deriveDecoder[T](using Mirror.Of[T]): Decoder[T] = MagnoliaDecoder.autoDerived[T]

}
