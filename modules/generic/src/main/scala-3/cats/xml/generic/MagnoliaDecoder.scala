package cats.xml.generic

import cats.data.NonEmptyList
import cats.xml.*
import cats.xml.codec.{Decoder, DecoderFailure}
import cats.xml.cursor.{Cursor, CursorFailure, FreeCursor}
import cats.xml.utils.generic.ParamName
import magnolia1.*
import magnolia1.CaseClass.Param

import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

class DerivedDecoder[T](delegate: Decoder[T]) extends Decoder[T] {
  def decodeCursorResult(cursorResult: Cursor.Result[Xml]): Decoder.Result[T] =
    delegate.decodeCursorResult(cursorResult)
}

object DerivedDecoder {
  inline def derived[T](using m: Mirror.Of[T]): DerivedDecoder[T] = new DerivedDecoder(
    MagnoliaDecoder.derived[T]
  )
  inline def derived[T <: AnyVal: XmlTypeInterpreter]: DerivedDecoder[T] =
    new DerivedDecoder(MagnoliaDecoder.derived[T])
}

object MagnoliaDecoder extends AutoDerivationHack[Decoder, XmlTypeInterpreter]:

  import cats.syntax.all.given

  val config: Configuration = Configuration.default

  override def join[T: XmlTypeInterpreter](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
    if (ctx.isValueClass && config.unwrapValueClasses) {
      ctx.params.head.typeclass.map(v => ctx.rawConstruct(Seq(v)))
    } else {
      val interpreter: XmlTypeInterpreter[T] = XmlTypeInterpreter[T]

      Decoder
        .fromCursor(c => {
          ctx.params.toList
            .mapFilter { (param: Param[Decoder, T]) =>
              given Decoder[param.PType] = param.typeclass

              interpreter
                .evalParam(ParamName[T](param.label))
                .mapFilter(paramInfo => {

                  // normalize element label
                  val normalizedLabel: String = paramInfo.labelMapper(param.label)

                  // find and decoder element
                  val result: Option[FreeCursor[param.PType]] = paramInfo.elemType match {
                    case XmlElemType.Attribute => Some(c.attr(normalizedLabel).as[param.PType])
                    case XmlElemType.Child     => Some(c.down(normalizedLabel).as[param.PType])
                    case XmlElemType.Text      => Some(c.text.as[param.PType])
                    case XmlElemType.Null      => None
                  }

                  // use fault parameter in case of missing element
                  if (config.useDefaults)
                    result.map(
                      _.recoverWith(
                        useDefaultParameterIfPresentToRecoverMissing[
                          Decoder,
                          T,
                          param.PType
                        ](param)
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

  override def split[T: XmlTypeInterpreter](ctx: SealedTrait[Typeclass, T]): Typeclass[T] =
    Decoder.instance(xml => {
      val subtypeTypeClass: Option[SealedTrait.Subtype[Typeclass, T, _]] = xml match {
        case node: XmlNode =>
          val target: String = (config.discriminatorAttrKey match {
            case Some(discriminatorAttrKey) => node.findAttr[String](discriminatorAttrKey)
            case None                       => node.label.some
          }).getOrElse(node.label)

          ctx.subtypes.find(_.typeInfo.short == target)
        case _ =>
          ctx.subtypes.headOption
      }

      subtypeTypeClass.map(_.typeclass.decode(xml)) match
        case Some(result) => result
        case None =>
          DecoderFailure
            .Custom(
              s"Cannot find a valid subtype for sealed trait ${ctx.typeInfo.toString}"
            )
            .invalidNel
    })

  // Internal error: unable to find the outer accessor symbol of class $read
  private def useDefaultParameterIfPresentToRecoverMissing[F[_], T, PT](
    param: Param[F, T]
  ): PartialFunction[NonEmptyList[CursorFailure], FreeCursor[PT]] = { failures =>
    if (failures.forall(_.isMissing))
      param.default match {
        case Some(value) =>
          FreeCursor.const[PT](value.asInstanceOf[PT].validNel[CursorFailure])
        case None => FreeCursor.failure(failures)
      }
    else
      FreeCursor.failure(failures)
  }

  inline def handleAnyVal[A <: AnyVal: XmlTypeInterpreter]: Decoder[A] = summonFrom {
    case f: WrapAnyVal[A, ?] =>
      summonFrom { case t: Decoder[f.From] =>
        handleAnyValImpl[A, f.From]
      }
  }

  def handleAnyValImpl[A <: AnyVal: XmlTypeInterpreter, B: Decoder](implicit
    f: WrapAnyVal[A, B]
  ): Decoder[A] = {
    Decoder[B].map(f.fn)
  }

case class WrapAnyVal[S <: AnyVal, T](fn: T => S) {
  type From = T
  type To   = S
}
