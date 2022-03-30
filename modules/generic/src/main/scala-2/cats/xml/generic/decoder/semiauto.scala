package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import magnolia1.*

object semiauto {

  type Typeclass[T] = Decoder[T]

  def deriveDecoder[T]: Decoder[T] =
    macro Magnolia.gen[T]

  def join[T](ctx: CaseClass[Decoder, T]): Decoder[T] =
    DecoderDerivation.join(ctx)
}
