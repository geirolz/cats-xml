package cats.xml.generic

import cats.xml.*
import cats.xml.codec.Encoder
import cats.xml.utils.generic.ParamName
import cats.xml.utils.impure
import magnolia1.*

import scala.compiletime.{summonFrom, summonInline}
import scala.deriving.Mirror

class DerivedEncoder[T](delegate: Encoder[T]) extends Encoder[T] {
  def encode(t: T): Xml = delegate.encode(t)
}

object DerivedEncoder {
  inline def derived[T](using m: Mirror.Of[T]): DerivedEncoder[T] = new DerivedEncoder(
    MagnoliaEncoder(Configuration.default).derived[T]
  )

  inline def derived[T <: AnyVal & Product: XmlTypeInterpreter]: DerivedEncoder[T] =
    new DerivedEncoder(MagnoliaEncoder(Configuration.default).derived[T])
}

class MagnoliaEncoder(config: Configuration)
    extends AutoDerivationHack[Encoder, XmlTypeInterpreter] {

  import cats.xml.syntax.*

  def join[T: XmlTypeInterpreter](
    ctx: CaseClass[Encoder, T]
  ): Encoder[T] = Encoder.of(t => {

    val nodeBuild: XmlNode.Node = XmlNode(ctx.typeInfo.short)
    def evaluateAndAppend(
      xml: Xml,
      param: CaseClass.Param[Encoder, T],
      paramInfo: XmlElemTypeParamInfo
    ): Unit =
      xml match {
        case XmlNull => ()
        case data: XmlData if paramInfo.elemType == XmlElemType.Attribute =>
          nodeBuild.unsafeMuteNode(
            _.appendAttr(
              XmlAttribute(
                key   = paramInfo.labelMapper(param.label),
                value = data
              )
            )
          )
        case data: XmlData if paramInfo.elemType == XmlElemType.Text =>
          nodeBuild.unsafeMuteNode(_.withText(data))
        case node: XmlNode.Node
            if paramInfo.elemType == XmlElemType.Child && config.useLabelsForNodes =>
          nodeBuild.unsafeMuteNode(
            _.appendChildren(node.withLabel(paramInfo.labelMapper(param.label)))
          )
        case node: XmlNode if paramInfo.elemType == XmlElemType.Child =>
          nodeBuild.unsafeMuteNode(_.appendChildren(node))
        case node: XmlData if paramInfo.elemType == XmlElemType.Child =>
          nodeBuild.unsafeMuteNode(
            _.appendChildren(
              XmlNode(paramInfo.labelMapper(param.label), content = NodeContent.text(node))
            )
          )
        case xml => throw new RuntimeException(debugMsg(xml, param, paramInfo))
      }

    ctx.parameters.foreach { param =>
      XmlTypeInterpreter[T]
        .evalParam(ParamName(param.label))
        .foreach((paramInfo: XmlElemTypeParamInfo) => {
          evaluateAndAppend(
            xml       = param.typeclass.encode(param.deref(t)),
            param     = param,
            paramInfo = paramInfo
          )
        })
    }

    nodeBuild
  })

  @impure
  def split[T: XmlTypeInterpreter](
    sealedTrait: SealedTrait[Encoder, T]
  ): Encoder[T] = Encoder {
    // special-case 'None' to avoid empty elements
    case None => Xml.Null
    case (a: T) =>
      sealedTrait.choose(a) { subtype =>
        val subTypeXml = subtype.typeclass.encode(subtype.cast(a))
        config.discriminatorAttrKey match {
          case Some(discriminatorAttrKey) =>
            val base = XmlNode(sealedTrait.typeInfo.short)
              .withAttrs(
                discriminatorAttrKey := subtype.typeInfo.short
              )

            subTypeXml match {
              case group: XmlNode.Group => base.withContent(group.content)
              case node: XmlNode.Node =>
                base
                  .appendAttrs(node.attributes)
                  .withContent(node.content)
              case attr: XmlAttribute => base.appendAttr(attr)
              case data: XmlData      => base.withText(data)
              case _                  => base
            }

          case None => subTypeXml
        }
      }
  }

  private def debugMsg[TC[_], T](
    xml: Xml,
    p: CaseClass.Param[TC, T],
    paramInfo: XmlElemTypeParamInfo
  ): String =
    s"""
       |Unable to handle an Xml element.
       |
       |Try to change your `XmlTypeInterpreter` implementation for type `${p.typeclass}` in order to
       |let the field `${p.label}` fall in one of the following supported cases:
       |
       |- XmlNode as XmlElemType.Child
       |- XmlData as XmlElemType.Attribute
       |- XmlData as XmlElemType.Text
       |
       |Current:
       |- ${xml.getClass.getSimpleName} as ${paramInfo.elemType}
       |
       |---------------------------------
       |Xml instance value: $xml
       |Xml instance type: ${xml.getClass.getName}
       |Field name: ${p.label}
       |Field type: ${p.typeclass}
       |Treated as: ${paramInfo.elemType}
       |""".stripMargin

  inline def handleAnyVal[A <: AnyVal & Product: XmlTypeInterpreter]: Encoder[A] = summonFrom {
    case f: FullEncSupp[A] => f.encoder
    case _                 => deriveAnyValSupport[A].encoder
  }

  inline implicit def deriveAnyValSupport[A <: AnyVal & Product: XmlTypeInterpreter]
    : FullEncSupp[A] =
    ${
      EncoderMacros.deriveAnyValSupportImpl[A]('this)
    }

  inline def handlePrimitive[A]: Encoder[A] = summonInline[Encoder[A]]
}

object EncoderMacros {

  import scala.quoted.*

  def deriveAnyValSupportImpl[A <: AnyVal](
    self: Expr[MagnoliaEncoder]
  )(using quotes: Quotes, tpe: Type[A]): Expr[FullEncSupp[A]] = {
    import quotes.*, quotes.reflect.*
    val wrapperSym  = TypeRepr.of[A].typeSymbol
    val constructor = wrapperSym.primaryConstructor
    val arg         = constructor.paramSymss.head.head
    val argName     = arg.name
    val theType = arg.tree match {
      case ValDef(_, tt: TypeTree, _) => tt
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "expecting AnyVal with Product to have a single constructor arg"
        )
    }
    theType.tpe.asType match {
      case '[t] =>
        val encoder =
          if (theType.symbol.isClassDef)
            TypeApply(
              Select.unique(self.asTerm, "mirrorDerived"),
              List(theType)
            )
          else
            TypeApply(
              Select.unique(self.asTerm, "noMirrorDerived"),
              List(theType)
            )

        val mtpe = MethodType(List("v"))(_ => List(TypeRepr.of[A]), _ => TypeRepr.of[t])

        def doUnapply = Lambda(
          Symbol.noSymbol,
          mtpe,
          {
            case (_, List(arg)) =>
              Select(Ref(arg.symbol), wrapperSym.fieldMember(argName))
            case _ =>
              quotes.reflect.report.errorAndAbort(
                "expecting AnyVal constructor to be called with a single arg"
              )
          }
        ).asExprOf[A => t]

        '{
          FullEncSupp[A](
            UnwrapAndSerde[A, t](${ doUnapply })(using ${ encoder.asExprOf[Encoder[t]] })
          )
        }.asExprOf[FullEncSupp[A]]
    }
  }
}

case class FullEncSupp[S <: AnyVal](unwrapAndSerde: UnwrapAndSerde[S, ?]) {
  def encoder = unwrapAndSerde.impl
}

case class UnwrapAndSerde[S <: AnyVal, T: Encoder](fn: S => T) {
  def impl = Encoder[T].contramap(fn)
}
