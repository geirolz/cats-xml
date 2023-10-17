//package cats.xml.generic
//
//import cats.data.NonEmptyList
//import cats.xml.*
//import cats.xml.codec.{Decoder, DecoderFailure}
//import cats.xml.cursor.{CursorFailure, FreeCursor}
//import cats.xml.generic.{Configuration, XmlElemType, XmlTypeInterpreter}
//import cats.xml.utils.generic.ParamName
//import magnolia1.*
//import magnolia1.CaseClass.Param
//
//import scala.deriving.Mirror
//
//object MagnoliaDecoder extends AutoDerivation[Decoder]:
//
//  import cats.syntax.all.given
//
//  val config: Configuration = Configuration.default
//
//  override def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
//    if (ctx.isValueClass && config.unwrapValueClasses) {
//      ctx.params.head.typeclass.map(v => ctx.rawConstruct(Seq(v)))
//    } else {
//      val interpreter: XmlTypeInterpreter[T] = XmlTypeInterpreter[T]
//
//      Decoder
//        .fromCursor(c => {
//          ctx.params.toList
//            .mapFilter { (param: Param[Decoder, T]) =>
//              given Decoder[param.PType] = param.typeclass
//
//              interpreter
//                .evalParam(ParamName[T](param.label))
//                .mapFilter(paramInfo => {
//
//                  // normalize element label
//                  val normalizedLabel: String = paramInfo.labelMapper(param.label)
//
//                  // find and decoder element
//                  val result: Option[FreeCursor[param.PType]] = paramInfo.elemType match {
//                    case XmlElemType.Attribute => Some(c.attr(normalizedLabel).as[param.PType])
//                    case XmlElemType.Child     => Some(c.down(normalizedLabel).as[param.PType])
//                    case XmlElemType.Text      => Some(c.text.as[param.PType])
//                    case XmlElemType.Null      => None
//                  }
//
//                  // use fault parameter in case of missing element
//                  if (config.useDefaults)
//                    result.map(
//                      _.recoverWith(
//                        useDefaultParameterIfPresentToRecoverMissing[Decoder, T, param.PType](param)
//                      )
//                    )
//                  else
//                    result
//                })
//            }
//            .toList
//            .sequence
//            .map(ctx.rawConstruct)
//        })
//    }
//
//  override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] =
//    Decoder.fromXml(xml => {
//      val subtypeTypeClass: Option[SealedTrait.Subtype[Typeclass, T, _]] = xml match
//        case node: XmlNode =>
//          ctx.subtypes.toList.find(_.typeInfo.short == node.label)
//        case _ =>
//          ctx.subtypes.headOption
//
//      subtypeTypeClass.map(_.typeclass.decode(xml)) match
//        case Some(result) => result
//        case None =>
//          DecoderFailure
//            .Custom(
//              s"Cannot find a valid subtype for sealed trait ${ctx.typeInfo.toString}"
//            )
//            .invalidNel
//    })
//
//  // Internal error: unable to find the outer accessor symbol of class $read
//  private def useDefaultParameterIfPresentToRecoverMissing[F[_], T, PT](
//    param: Param[F, T]
//  ): PartialFunction[NonEmptyList[CursorFailure], FreeCursor[PT]] = { failures =>
//    if (failures.forall(_.isMissing))
//      param.default match {
//        case Some(value) =>
//          FreeCursor.const[Xml, PT](value.asInstanceOf[PT].validNel[CursorFailure])
//        case None => FreeCursor.failure(failures)
//      }
//    else
//      FreeCursor.failure(failures)
//  }
