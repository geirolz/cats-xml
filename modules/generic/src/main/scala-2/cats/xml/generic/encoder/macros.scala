package cats.xml.generic.encoder

import cats.xml.codec.Encoder
import cats.xml.generic.XmlTypeInterpreter
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object auto {

  type Typeclass[T] = Encoder[T]

  implicit def deriveEncoder[T]: Typeclass[T] = macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    EncoderDerivation.join(caseClass)

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    EncoderDerivation.split(sealedTrait)
}

object semiauto {

  type Typeclass[T] = Encoder[T]

  def deriveEncoder[T]: Encoder[T] =
    macro Magnolia.gen[T]

  def join[T: XmlTypeInterpreter](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    EncoderDerivation.join(caseClass)

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    EncoderDerivation.split(sealedTrait)
}
