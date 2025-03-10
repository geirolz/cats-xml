package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder}
import scala.deriving.Mirror

object semiauto {
  private final val foo = new MagnoliaEncoder(Configuration.default)

  inline def deriveEncoder[T](using Mirror.Of[T]): Encoder[T] = foo.derived[T]
}
