package cats.xml.generic

import cats.data.NonEmptyList
import cats.xml.codec.{Decoder, DecoderFailure}
import cats.xml.cursor.{CursorFailure, FreeCursor}
import cats.xml.{Xml, XmlNode}
import cats.xml.utils.generic.ParamName
import magnolia1.{CaseClass, Param, SealedTrait, Subtype}

object MagnoliaDecoder {

  import cats.implicits.*

  private[generic] def join[T: XmlTypeInterpreter](
    ctx: CaseClass[Decoder, T],
    config: Configuration
  ): Decoder[T] =
    if (ctx.isValueClass && config.unwrapValueClasses) {
      ctx.parameters.head.typeclass.map(v => ctx.rawConstruct(Seq(v)))
    } else {
      val interpreter: XmlTypeInterpreter[T] = XmlTypeInterpreter[T]

      Decoder
        .fromCursor(c => {
          ctx.parameters
            .mapFilter { (param: Param[Decoder, T]) =>
              implicit val pdec: Decoder[param.PType] = param.typeclass

              interpreter
                .evalParam(ParamName[T](param.label))
                .mapFilter(paramInfo => {

                  // normalize element label
                  val normalizedLabel: String = paramInfo.labelMapper(param.label)

                  // find and decoder element
                  val result: Option[FreeCursor[Xml, param.PType]] = paramInfo.elemType match {
                    case XmlElemType.Attribute => Some(c.attr(normalizedLabel).as[param.PType])
                    case XmlElemType.Child     => Some(c.down(normalizedLabel).as[param.PType])
                    case XmlElemType.Text      => Some(c.text.as[param.PType])
                    case XmlElemType.Null      => None
                  }

                  // use default parameter in case of missing element
                  if (config.useDefaults)
                    result.map(
                      _.recoverWith(
                        useDefaultParameterIfPresentToRecoverMissing[Decoder, T, param.PType](param)
                      )
                    )
                  else
                    result
                })
            }
            .toList
            .sequence
            .map(ctx.rawConstruct)
        })
    }

  private[generic] def split[T](
    ctx: SealedTrait[Decoder, T],
    config: Configuration
  ): Decoder[T] =
    Decoder.instance(xml => {

      val subtypeTypeClass: Option[Subtype[Decoder, T]] = xml match {
        case node: XmlNode =>
          val target: String = (config.discriminatorAttrKey match {
            case Some(discriminatorAttrKey) => node.findAttr[String](discriminatorAttrKey)
            case None                       => node.label.some
          }).getOrElse(node.label)

          ctx.subtypes.find(_.typeName.short == target)
        case _ =>
          ctx.subtypes.headOption
      }

      subtypeTypeClass.map(_.typeclass.decode(xml)) match {
        case Some(result) => result
        case None =>
          DecoderFailure
            .Custom(
              s"Cannot find a valid subtype for sealed trait ${ctx.typeName.full}. Config: $config"
            )
            .invalidNel
      }
    })

  // Internal error: unable to find the outer accessor symbol of class $read
  private def useDefaultParameterIfPresentToRecoverMissing[F[_], T, PT](
    param: Param[F, T]
  ): PartialFunction[NonEmptyList[CursorFailure], FreeCursor[Xml, PT]] = { failures =>
    if (failures.forall(_.isMissing))
      param.default match {
        case Some(value) =>
          FreeCursor.const[Xml, PT](value.asInstanceOf[PT].validNel[CursorFailure])
        case None => FreeCursor.failure(failures)
      }
    else
      FreeCursor.failure(failures)
  }
}
