package cats.xml.generic

import cats.xml.*
import cats.xml.codec.Encoder
import cats.xml.utils.generic.ParamName
import cats.xml.utils.impure
import magnolia1.*

import scala.compiletime.{summonFrom, summonInline}

class MagnoliaEncoder(config: Configuration)
    extends AutoDerivationHack[Encoder, XmlTypeInterpreter] {

  import cats.xml.syntax.*

  def join[T: Ps](
    ctx: CaseClass[Typeclass, T]
  ): Typeclass[T] = Encoder.of(t => {

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
  ): Encoder[T] = Encoder { (a: T) =>
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
    case f: UnwrapAnyVal[A, ?] =>
      summonFrom { case t: Encoder[f.To] =>
        handleAnyValImpl[A, f.To]
      }
  }

  inline def handleAnyValImpl[A <: AnyVal & Product: XmlTypeInterpreter, B: Encoder](implicit
    f: UnwrapAnyVal[A, B]
  ): Encoder[A] = {
    val x = XmlTypeInterpreter[A]
    Encoder[B].contramap(f.fn)
  }
  inline def handlePrimitive[A]: Encoder[A] = summonInline[Encoder[A]]
}

case class UnwrapAnyVal[S <: AnyVal, T](fn: S => T) {
  type From = S
  type To   = T
}
