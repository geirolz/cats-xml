package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import cats.xml.generic.{Configuration, MagnoliaEncoder, XmlTypeInterpreter}
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object auto {

  type Typeclass[T] = Encoder[T]

  implicit def deriveEncoder[T]: Typeclass[T] = macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    MagnoliaEncoder.join(caseClass, Configuration.default)

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    MagnoliaEncoder.split(sealedTrait, Configuration.default)
}
