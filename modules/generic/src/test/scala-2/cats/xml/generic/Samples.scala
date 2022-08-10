package cats.xml.generic

object Samples {
  case class ValueClass(value: String) extends AnyVal

  case class Bar(field1: String, field2: BigDecimal)

  case class Foo(
    primitiveField: Double = 666d,
    valueClass: ValueClass,
    bar: Bar,
    missingField: Option[String],
    missingNode: Option[Bar]
  )
}
