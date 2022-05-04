package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import magnolia1.*

import scala.reflect.runtime.universe.*
import scala.reflect.ClassTag

object auto {

  type Typeclass[T] = Decoder[T]

  implicit def deriveDecoder[T]: Decoder[T] =
    macro Magnolia.gen[T]

  def join[T: ClassTag: TypeTag](ctx: CaseClass[Decoder, T]): Decoder[T] =
    DecoderDerivation.join(ctx)
}
