package cats.xml.generic

import cats.xml.codec.Decoder

object Samples {

  case class ValueClass(value: String) extends AnyVal derives DerivedDecoder
  case class InsideValueClass(v: String) derives DerivedDecoder
  case class ValueClass2(value: InsideValueClass) extends AnyVal derives DerivedDecoder
  case class Bar(field1: String, field2: BigDecimal) derives DerivedDecoder

  case class Foo(
    primitiveField: Double = 666d,
    valueClass: ValueClass,
    valueClass2: ValueClass2,
    bar: Bar,
    missingField: Option[String],
    missingNode: Option[Bar]
  ) derives DerivedDecoder

  sealed trait Vehicle {
    val wheelCount: Int
  }
  case class Car(wheelCount: Int) extends Vehicle
  case class Bike(wheelCount: Int) extends Vehicle
  case class Motorcycle(wheelCount: Int) extends Vehicle
}
