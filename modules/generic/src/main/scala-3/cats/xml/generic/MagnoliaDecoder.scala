package cats.xml.generic

import cats.data.NonEmptyList
import cats.xml.*
import cats.xml.codec.{Decoder, DecoderFailure}
import cats.xml.cursor.{Cursor, CursorFailure, FreeCursor}
import cats.xml.generic.decoder.semiauto
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
    semiauto.deriveDecoder[T]
  )
  inline def derived[T <: AnyVal & Product: XmlTypeInterpreter]: DerivedDecoder[T] =
    new DerivedDecoder(semiauto.deriveDecoder[T])
}

class MagnoliaDecoder(config: Configuration)
    extends AutoDerivationHack[Decoder, XmlTypeInterpreter]:

  import cats.syntax.all.given

  override def join[T: XmlTypeInterpreter](ctx: CaseClass[Decoder, T]): Decoder[T] =
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

  override def split[T: XmlTypeInterpreter](ctx: SealedTrait[Decoder, T]): Decoder[T] =
    Decoder.instance(xml => {
      val subtypeTypeClass: Option[SealedTrait.Subtype[Decoder, T, _]] = xml match {
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

  inline def handleAnyVal[A <: AnyVal & Product: XmlTypeInterpreter]: Decoder[A] = summonFrom {
    case f: FullSupp[A] => f.decoder
    case _              => deriveAnyValSupport[A].decoder
  }

  // Since there's no 'Mirror.Of' for user-defined AnyVals, we attempt to summon the Decoder for the wrapped type
  // from within a macro, and map it to a decoder for the wrapper with the wrapper's constructor
  inline implicit def deriveAnyValSupport[A <: AnyVal & Product: XmlTypeInterpreter]: FullSupp[A] =
    ${
      DecoderMacros.deriveAnyValSupportImpl[A]('this)
    }
  inline def handlePrimitive[A]: Decoder[A] = summonInline[Decoder[A]]

object DecoderMacros {
  import scala.quoted.*

  def deriveAnyValSupportImpl[A <: AnyVal](
    self: Expr[MagnoliaDecoder]
  )(using quotes: Quotes, tpe: Type[A]): Expr[FullSupp[A]] = {
    import quotes.*, quotes.reflect.*
    val wrapperSym  = TypeRepr.of[A].typeSymbol
    val constructor = wrapperSym.primaryConstructor
    val theType = constructor.paramSymss.head.head.tree match {
      case ValDef(_, tt: TypeTree, _) => tt
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "expecting AnyVal with Product to have a single constructor arg"
        )
    }
    theType.tpe.asType match {
      case '[t] =>
        val decoder =
          if (theType.symbol.isClassDef)
            Implicits.search(TypeRepr.of[scala.deriving.Mirror.Of[t]]) match {
              case mirror: ImplicitSearchSuccess =>
                Apply(
                  TypeApply(
                    Select.unique(self.asTerm, "mirrorDerived"),
                    List(theType)
                  ),
                  List(mirror.tree)
                )
            }
          else
            TypeApply(
              Select.unique(self.asTerm, "noMirrorDerived"),
              List(theType)
            )

        val mtpe = MethodType(List("v"))(_ => List(TypeRepr.of[t]), _ => TypeRepr.of[A])
        def doApply = Lambda(
          Symbol.noSymbol,
          mtpe,
          {
            case (_, List(arg)) =>
              Apply(
                Select(New(TypeIdent(wrapperSym)), constructor),
                List(Ref(arg.symbol))
              )
            case _ =>
              quotes.reflect.report.errorAndAbort(
                "expecting AnyVal constructor to be called with a single arg"
              )
          }
        ).asExprOf[t => A]

        '{
          FullSupp[A](
            WrapAndSerde[A, t](${ doApply })(using ${ decoder.asExprOf[Decoder[t]] })
          )
        }.asExprOf[FullSupp[A]]
    }
  }
}
case class FullSupp[S <: AnyVal](wrapAndSerde: WrapAndSerde[S, ?]) {
  def decoder = wrapAndSerde.impl
}
case class WrapAndSerde[S <: AnyVal, T: Decoder](fn: T => S) {
  def impl = Decoder[T].map(fn)
}
