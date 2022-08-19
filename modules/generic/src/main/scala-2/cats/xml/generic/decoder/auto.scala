package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}
import magnolia1.{CaseClass, Magnolia}

object auto {

  type Typeclass[T] = Decoder[T]

  implicit def deriveDecoder[T]: Typeclass[T] = macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.join(ctx, Configuration.default)
}
