package cats.xml.generic.decoder.configured

import cats.xml.codec.Decoder
import cats.xml.generic.{Configuration, MagnoliaDecoder, XmlTypeInterpreter}
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object semiauto {

  type Typeclass[T] = Decoder[T]

  def deriveConfiguredDecoder[T]: Typeclass[T] = macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](caseClass: CaseClass[Typeclass, T])(implicit
    config: Configuration
  ): Typeclass[T] =
    MagnoliaDecoder.join(caseClass, config)

  def split[T: XmlTypeInterpreter](sealedTrait: SealedTrait[Typeclass, T])(implicit
    config: Configuration
  ): Typeclass[T] =
    MagnoliaDecoder.split(sealedTrait, config)
}
