//package cats.xml.generic
//
//import cats.xml.codec.Decoder
//
//object Samples {
//
//  import cats.xml.generic.MagnoliaDecoder.given
//  import cats.xml.generic.MagnoliaDecoder.*
//
//  case class ValueClass(value: String) extends AnyVal derives Decoder
//  case class Bar(field1: String, field2: BigDecimal) derives Decoder
//
//  case class Foo(
//    primitiveField: Double = 666d,
//    valueClass: ValueClass,
//    bar: Bar,
//    missingField: Option[String],
//    missingNode: Option[Bar]
//  ) derives Decoder
//}
