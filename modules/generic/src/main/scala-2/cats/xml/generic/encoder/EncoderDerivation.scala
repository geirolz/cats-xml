package cats.xml.generic.encoder

import cats.xml.{XmlAttribute, XmlData, XmlNode}
import cats.xml.codec.Encoder
import magnolia1.CaseClass

object EncoderDerivation {
  def join[T](ctx: CaseClass[Encoder, T]): Encoder[T] =
    Encoder.of(t => {

      val node = XmlNode(ctx.typeName.short)
      ctx.parameters.foreach(p =>
        p.typeclass.encode(p.dereference(t)) match {
          case data: XmlData => node.mute(_.appendAttr(XmlAttribute(p.label, data)))
          case node: XmlNode => node.mute(_.appendChild(node))
          case _             => ()
        }
      )
      node
    })
}
