//package cats.xml.generic.decoder
//
//import cats.xml.codec.Decoder
//import magnolia1.CaseClass
//
//object DecoderMacro {
////
////  type Typeclass[A] = Decoder[A]
////
////  import cats.implicits.*
////
////  def combine[A](caseClass: CaseClass[Typeclass, A]): Typeclass[A] =
////    Decoder.fromCursor(c => {
////      caseClass.parameters
////        .map(p => {
////          p.typeName.full match {
////            case "String" => c.attr(p.label).as[String]
////            case _        => ???
////          }
////        })
////        .toList
////        .sequence
////        .map(caseClass.rawConstruct)
////    })
//
////  def dispatch[A](sealedTrait: SealedTrait[Typeclass, A]): Typeclass[A] = {
////    Decoder.fromCursor(c => {
////      sealedTrait.subtypes.head.typeclass.decode(???)
////    })
////  }
//  //
////
////    create[A] {
////    case JsObject(jsonMap) if jsonMap.contains("type") && jsonMap.contains("value") =>
////      val typeName = jsonMap("type") match {
////        case JsString(value) => value
////        case other           => ErrorOr.singleError(s"Expected JsString in type field, got: $other")
////      }
////
////      sealedTrait.subtypes.find(_.typeName.full == typeName) match {
////        case None =>
////          ErrorOr.singleError(
////            s"${sealedTrait.typeName.full} does not contain a member of type $typeName"
////          )
////        case Some(subType) => subType.typeclass.read(jsonMap("value"))
////      }
////
////    case other =>
////      ErrorOr.singleError(s"Expected JsObject with `type` and `value` fields, got: $other")
////  }
////  }
//}
