package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.XmlTypeInterpreter
import magnolia1.*

object auto {

  type Typeclass[T] = Decoder[T]

  implicit def deriveDecoder[T]: Decoder[T] =
    macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](ctx: CaseClass[Decoder, T]): Decoder[T] =
    DecoderDerivation.join(ctx)
}

object semiauto {

  type Typeclass[T] = Decoder[T]

  def deriveDecoder[T]: Decoder[T] =
    macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](ctx: CaseClass[Decoder, T]): Decoder[T] =
    DecoderDerivation.join(ctx)
}
