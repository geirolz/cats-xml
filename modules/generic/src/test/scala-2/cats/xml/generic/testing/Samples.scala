package cats.xml.generic.testing

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

  sealed trait Vehicle {
    val wheelCount: Int
  }
  case class Car(wheelCount: Int) extends Vehicle
  case class Bike(wheelCount: Int) extends Vehicle
  case class Motorcycle(wheelCount: Int) extends Vehicle
}
