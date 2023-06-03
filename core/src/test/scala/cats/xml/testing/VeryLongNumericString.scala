package cats.xml.testing

import org.scalacheck.{Arbitrary, Gen}

case class VeryLongNumericString private (str: String) extends AnyVal
object VeryLongNumericString {

  private[VeryLongNumericString] def apply(str: String): VeryLongNumericString =
    new VeryLongNumericString(str)

  implicit val arb: Arbitrary[VeryLongNumericString] =
    Arbitrary(
      Gen.numStr.suchThat(_.length >= 31).map(VeryLongNumericString(_))
    )
}
