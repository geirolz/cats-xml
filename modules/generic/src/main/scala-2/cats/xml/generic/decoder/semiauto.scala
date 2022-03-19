//package cats.xml.generic.decoder
//
//import cats.xml.codec.Decoder
//import magnolia1.{CaseClass, Magnolia}
//
//object semiauto {
//
//  type Typeclass[T] = Decoder[T]
//
//  def deriveDecoder[T]: Decoder[T] = macro Magnolia.gen[T]
//
//  def combine[T](caseClass: CaseClass[Decoder, T]): Decoder[T] =
//    DecoderMacro.combine(caseClass)
////
////  def dispatch[A](sealedTrait: SealedTrait[Typeclass, A]): Typeclass[A] =
////    DecoderMacro.dispatch(sealedTrait)
//}
