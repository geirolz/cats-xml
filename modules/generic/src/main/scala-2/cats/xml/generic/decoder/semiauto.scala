package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object semiauto {

  type Typeclass[T] = Decoder[T]

  def deriveDecoder[T]: Typeclass[T] = macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.join(ctx, Configuration.default)

  def split[T: XmlTypeInterpreter](ctx: SealedTrait[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.split(ctx, Configuration.default)
}
