package cats.xml.generic.decoder

import cats.xml.codec.Decoder
import magnolia1.CaseClass

object DecoderDerivation {

  import cats.implicits.*

  private val attributesTypes: List[String] = List(
    "Unit",
    "String",
    "Boolean",
    "Char",
    "Int",
    "Long",
    "Float",
    "Double",
    "BigDecimal"
  )

  private val monadTypes: List[String] = List(
    "Either",
    "Option",
    "Try"
  )

  // product
  def join[T](ctx: CaseClass[Decoder, T]): Decoder[T] =
    Decoder
      .fromCursor(c => {
        ctx.parameters
          .map(p => {

            implicit val pdec: Decoder[p.PType] = p.typeclass
            val isPrimitive                     = attributesTypes.contains(p.typeName.short)
            val isWrapper                       = monadTypes.contains(p.typeName.short)
            val isWrapperOfPrimitive = isWrapper &&
              attributesTypes.contains(p.typeName.typeArguments.head.short)

            if (isPrimitive || isWrapperOfPrimitive) {
              c.attr(p.label).as[p.PType]
            } else {
              c.down(p.label).as[p.PType]
            }
          })
          .toList
          .sequence
          .map(ctx.rawConstruct)
      })
}
