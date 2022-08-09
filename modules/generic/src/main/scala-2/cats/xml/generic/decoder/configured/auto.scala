package cats.xml.generic.decoder.configured

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}
import magnolia1.{CaseClass, Magnolia}

object auto {

  type Typeclass[T] = Decoder[T]

  implicit def deriveConfiguredDecoder[T]: Typeclass[T] =
    macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](ctx: CaseClass[Typeclass, T])(implicit
    config: Configuration
  ): Typeclass[T] =
    MagnoliaDecoder.join(ctx, config)
}
