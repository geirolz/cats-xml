package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import magnolia1.*

object semiauto {

  type Typeclass[T] = Encoder[T]

  def deriveEncoder[T]: Encoder[T] =
    macro Magnolia.gen[T]

  def join[T](ctx: CaseClass[Encoder, T]): Encoder[T] =
    EncoderDerivation.join(ctx)
}
