package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import magnolia1.{CaseClass, Magnolia}

object auto {

  type Typeclass[T] = Encoder[T]

  implicit def deriveEncoder[T]: Encoder[T] =
    macro Magnolia.gen[T]

  def join[T](ctx: CaseClass[Encoder, T]): Encoder[T] =
    EncoderDerivation.join(ctx)
}
