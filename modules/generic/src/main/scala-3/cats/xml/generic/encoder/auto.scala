package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder}
import scala.deriving.Mirror

object auto {
  private final val foo = new MagnoliaEncoder(Configuration.default)

  inline given deriveEncoder[T](using Mirror.Of[T]): Encoder[T] = foo.autoDerived[T]

}
