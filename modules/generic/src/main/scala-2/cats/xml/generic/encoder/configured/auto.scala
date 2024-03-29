package cats.xml.generic.encoder.configured

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object auto {

  type Typeclass[T] = Encoder[T]

  implicit def deriveConfiguredEncoder[T]: Typeclass[T] = macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](caseClass: CaseClass[Typeclass, T])(implicit
    config: Configuration
  ): Typeclass[T] =
    MagnoliaEncoder.join(caseClass, config)

  def split[T: XmlTypeInterpreter](sealedTrait: SealedTrait[Typeclass, T])(implicit
    config: Configuration
  ): Typeclass[T] =
    MagnoliaEncoder.split(sealedTrait, config)
}
