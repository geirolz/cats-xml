package cats.xml.testing.arbitrary

import cats.xml.XmlData.*
import cats.xml.testing.{DataSize, XmlGen}
import cats.xml.{Xml, XmlAttribute, XmlData}
import org.scalacheck.{Arbitrary, Gen}

object XmlArbitrary {

  implicit val arbXmlNode: Arbitrary[Xml] = Arbitrary(
    Gen.oneOf(
      XmlGen.genXmlNode(DataSize.S),
      XmlGen.genXmlNode(DataSize.M),
      XmlGen.genXmlNode(DataSize.L),
      XmlGen.genXmlData,
      XmlGen.genAttribute()
    )
  )

  implicit val arbXmlAttribute: Arbitrary[XmlAttribute] =
    Arbitrary(XmlGen.genAttribute())

  implicit val arbXmlString: Arbitrary[XmlString] =
    Arbitrary(XmlGen.genXmlString)

  implicit val arbXmlChar: Arbitrary[XmlChar] =
    Arbitrary(XmlGen.genXmlChar)

  implicit val arbXmlBool: Arbitrary[XmlBool] =
    Arbitrary(XmlGen.genXmlBool)

  implicit val arbXmlLong: Arbitrary[XmlLong] =
    Arbitrary(XmlGen.genXmlLong)

  implicit val arbXmlFloat: Arbitrary[XmlFloat] =
    Arbitrary(XmlGen.genXmlFloat)

  implicit val arbXmlDouble: Arbitrary[XmlDouble] =
    Arbitrary(XmlGen.genXmlDouble)

  implicit val arbXmlBigDecimal: Arbitrary[XmlBigDecimal] =
    Arbitrary(XmlGen.genXmlBigDecimal)

  implicit val arbXmlData: Arbitrary[XmlData] =
    Arbitrary(XmlGen.genXmlData)
}
