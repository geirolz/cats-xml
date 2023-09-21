package cats.xml.testing

import cats.xml.Xml
import org.scalacheck.{Arbitrary, Gen}

case class XmlValidName(value: String) extends AnyVal
object XmlValidName {

  private def apply(value: String): XmlValidName =
    new XmlValidName(value)

  def genXmlValidName(maxSize: Int = 10): Gen[XmlValidName] =
    Gen.lzy(
      Gen
        .choose(1, maxSize)
        .flatMap(Gen.stringOfN(_, Gen.alphaChar))
        .filter(Xml.isValidXmlName)
        .map(XmlValidName(_))
    )

  implicit val xmlValidNameStringArbitrary: Arbitrary[XmlValidName] =
    Arbitrary { genXmlValidName() }
}
