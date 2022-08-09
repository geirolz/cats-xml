package cats.xml.generic.encoder.configured

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object semiauto {

  type Typeclass[T] = Encoder[T]

  def deriveConfiguredEncoder[T]: Encoder[T] =
    macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](caseClass: CaseClass[Typeclass, T])(implicit
    config: Configuration
  ): Typeclass[T] =
    MagnoliaEncoder.join(caseClass, config)

  def split[T](sealedTrait: SealedTrait[Typeclass, T])(implicit
    config: Configuration
  ): Typeclass[T] =
    MagnoliaEncoder.split(sealedTrait, config)
}
