package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import cats.xml.generic.ClassXmlInterpreter
import cats.xml.generic.ClassXmlInterpreter.XmlElemType
import magnolia1.CaseClass

object DecoderDerivation {

  import cats.implicits.*

  // product
  def join[T: ClassXmlInterpreter](ctx: CaseClass[Decoder, T]): Decoder[T] =
    if (ctx.isValueClass) {
      ctx.parameters.head.typeclass.map(v => ctx.rawConstruct(Seq(v)))
    } else {
      Decoder
        .fromCursor(c => {
          ctx.parameters
            .mapFilter { param =>
              implicit val pdec: Decoder[param.PType] = param.typeclass
              ClassXmlInterpreter[T].evalParameter(param.label).map {
                case XmlElemType.Attribute => c.attr(param.label).as[param.PType]
                case XmlElemType.Text      => c.text.as[param.PType]
                case XmlElemType.Child     => c.down(param.label).as[param.PType]
              }
            }
            .toList
            .sequence
            .map(ctx.rawConstruct)
        })
    }

}
