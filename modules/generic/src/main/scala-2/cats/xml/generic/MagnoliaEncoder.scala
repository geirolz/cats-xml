package cats.xml.generic

import cats.xml.{Xml, XmlAttribute, XmlData, XmlNode}
import cats.xml.codec.Encoder
import cats.xml.Xml.XmlNull
import magnolia1.{CaseClass, Param, SealedTrait}

import scala.annotation.unused

object MagnoliaEncoder {

  private[generic] def join[T: XmlTypeInterpreter](
    ctx: CaseClass[Encoder, T],
    @unused config: Configuration
  ): Encoder[T] = {
    if (ctx.isValueClass) {
      val valueParam: Param[Encoder, T] = ctx.parameters.head
      valueParam.typeclass.contramap[T](valueParam.dereference(_))
    } else {
      val interpreter: XmlTypeInterpreter[T] = XmlTypeInterpreter[T]

      Encoder.of(t => {

        val nodeBuild = XmlNode(ctx.typeName.short)

        def evaluateAndAppend(
          xml: Xml,
          param: Param[Encoder, T],
          paramInfo: XmlElemTypeParamInfo
        ): Unit =
          xml match {
            case XmlNull => ()
            case data: XmlData if paramInfo.elemType == XmlElemType.Attribute =>
              nodeBuild.mute(
                _.appendAttr(
                  XmlAttribute(
                    key   = paramInfo.labelMapper(param.label),
                    value = data
                  )
                )
              )
            case data: XmlData if paramInfo.elemType == XmlElemType.Text =>
              nodeBuild.mute(_.withText(data))
            case node: XmlNode if paramInfo.elemType == XmlElemType.Child =>
              nodeBuild.mute(_.appendChild(node))
            case xml => throw new RuntimeException(debugMsg(xml, param, paramInfo))
          }

        ctx.parameters.foreach(param =>
          interpreter
            .evalParam(ParamName(param.label))
            .foreach((paramInfo: XmlElemTypeParamInfo) => {
              evaluateAndAppend(
                xml       = param.typeclass.encode(param.dereference(t)),
                param     = param,
                paramInfo = paramInfo
              )
            })
        )

        nodeBuild
      })
    }
  }

  private[generic] def split[T: XmlTypeInterpreter](
    sealedTrait: SealedTrait[Encoder, T],
    @unused config: Configuration
  ): Encoder[T] = { (a: T) =>
    {
      sealedTrait.split(a) { subtype =>
        subtype.typeclass.encode(subtype.cast(a))
      }
    }
  }

  private def debugMsg[TC[_], T](
    xml: Xml,
    p: Param[TC, T],
    paramInfo: XmlElemTypeParamInfo
  ): String =
    s"""
       |Unable to handle an Xml element.
       |
       |Try to change your `XmlTypeInterpreter` implementation for type `${p.typeName.full}` in order to 
       |let the field `${p.label}` falls in one of the following supported cases:
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
       |Field type: ${p.typeName.full}
       |Treated as: ${paramInfo.elemType}
       |""".stripMargin
}
