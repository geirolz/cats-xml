package cats.xml.generic.encoder

import cats.xml.{XmlAttribute, XmlData, XmlNode}
import cats.xml.codec.Encoder
import cats.xml.generic.{ParamName, XmlElemType, XmlTypeInterpreter}
import cats.xml.Xml.XmlNull
import magnolia1.{CaseClass, Param}

object EncoderDerivation {

  def join[T: XmlTypeInterpreter](ctx: CaseClass[Encoder, T]): Encoder[T] = {
    if (ctx.isValueClass) {
      val valueParam: Param[Encoder, T] = ctx.parameters.head
      valueParam.typeclass.contramap[T](valueParam.dereference(_))
    } else {
      val interpreter: XmlTypeInterpreter[T] = XmlTypeInterpreter[T]
      Encoder.of(t => {
        val nodeBuild = XmlNode(ctx.typeName.short)
        ctx.parameters.foreach(p =>
          interpreter
            .evalParam(ParamName(p.label))
            .foreach(paramInfo => {
              p.typeclass.encode(p.dereference(t)) match {
                case XmlNull => ()
                case data: XmlData if paramInfo.elemType == XmlElemType.Attribute =>
                  nodeBuild.mute(
                    _.appendAttr(
                      XmlAttribute(
                        key   = paramInfo.labelMapper(p.label),
                        value = data
                      )
                    )
                  )
                case data: XmlData if paramInfo.elemType == XmlElemType.Text =>
                  nodeBuild.mute(_.withText(data))
                case node: XmlNode => nodeBuild.mute(_.appendChild(node))
                case _             => ()
              }
            })
        )

        nodeBuild
      })
    }
  }
}
