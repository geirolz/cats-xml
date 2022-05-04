package cats.xml.generic.encoder

import cats.xml.{XmlAttribute, XmlData, XmlNode}
import cats.xml.codec.Encoder
import cats.xml.generic.{XmlElemType, XmlTypeInterpreter}
import magnolia1.CaseClass

object EncoderDerivation {

  def join[T: XmlTypeInterpreter](ctx: CaseClass[Encoder, T]): Encoder[T] = {
    val interpreter: XmlTypeInterpreter[T] = XmlTypeInterpreter[T]
    Encoder.of(t => {
      val node = XmlNode(ctx.typeName.short)
      ctx.parameters.foreach(p =>
        interpreter
          .evalParam(p.label)
          .foreach(paramInfo => {
            p.typeclass.encode(p.dereference(t)) match {
              case data: XmlData if paramInfo.elemType == XmlElemType.Attribute =>
                node.mute(
                  _.appendAttr(
                    XmlAttribute(
                      key   = paramInfo.labelMapper(p.label),
                      value = data
                    )
                  )
                )
              case data: XmlData if paramInfo.elemType == XmlElemType.Text =>
                node.mute(_.withText(data))
              case node: XmlNode => node.mute(_.appendChild(node))
              case _             => ()
            }
          })
      )

      node
    })
  }
}
