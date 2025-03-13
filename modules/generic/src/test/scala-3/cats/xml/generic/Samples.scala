package cats.xml.generic

import cats.xml.codec.Decoder

object Samples {

//  import cats.xml.generic.MagnoliaDecoder.given
//  import cats.xml.generic.MagnoliaDecoder.*

  case class ValueClass(value: String) extends AnyVal derives DerivedDecoder
  case class Bar(field1: String, field2: BigDecimal) derives DerivedDecoder

  case class Foo(
    primitiveField: Double = 666d,
    valueClass: ValueClass,
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
