package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.{XmlElemType, XmlTypeInterpreter}
import magnolia1.CaseClass

object DecoderDerivation {

  import cats.implicits.*

  // product
  def join[T: XmlTypeInterpreter](ctx: CaseClass[Decoder, T]): Decoder[T] =
    if (ctx.isValueClass) {
      ctx.parameters.head.typeclass.map(v => ctx.rawConstruct(Seq(v)))
    } else {
      val interpreter: XmlTypeInterpreter[T] = XmlTypeInterpreter[T]

      Decoder
        .fromCursor(c => {
          ctx.parameters
            .mapFilter { param =>
              implicit val pdec: Decoder[param.PType] = param.typeclass

              interpreter
                .evalParam(param.label)
                .map(paramInfo => {
                  val normalizedLabel = paramInfo.labelMapper(param.label)
                  paramInfo.elemType match {
                    case XmlElemType.Attribute => c.attr(normalizedLabel).as[param.PType]
                    case XmlElemType.Child     => c.down(normalizedLabel).as[param.PType]
                    case XmlElemType.Text      => c.text.as[param.PType]
                  }
                })
            }
            .toList
            .sequence
            .map(ctx.rawConstruct)
        })
    }

}
