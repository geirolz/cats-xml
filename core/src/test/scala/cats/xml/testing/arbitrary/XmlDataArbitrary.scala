package cats.xml.testing.arbitrary

import cats.xml.XmlData
import cats.xml.XmlData.*
import org.scalacheck.{Arbitrary, Gen}

object XmlDataArbitrary {

  implicit val arbXmlString: Arbitrary[XmlString] =
    Arbitrary {
      Gen.asciiPrintableStr
        .map(XmlData.fromString)
        .map(_.asInstanceOf[XmlString])
    }

  implicit val arbXmlChar: Arbitrary[XmlChar] =
    Arbitrary {
      Gen.alphaNumChar
        .map(XmlData.fromChar)
        .map(_.asInstanceOf[XmlChar])
    }

  implicit val arbXmlBool: Arbitrary[XmlBool] =
    Arbitrary {
      Arbitrary.arbBool.arbitrary
        .map(XmlData.fromBoolean)
        .map(_.asInstanceOf[XmlBool])
    }

  implicit val arbXmlInt: Arbitrary[XmlInt] =
    Arbitrary {
      Arbitrary.arbInt.arbitrary
        .map(XmlData.fromInt)
        .map(_.asInstanceOf[XmlInt])
    }

  implicit val arbXmlLong: Arbitrary[XmlLong] =
    Arbitrary {
      Gen.long
        .map(XmlData.fromLong)
        .map(_.asInstanceOf[XmlLong])
    }

  implicit val arbXmlFloat: Arbitrary[XmlFloat] =
    Arbitrary {
      Arbitrary.arbFloat.arbitrary
        .map(XmlData.fromFloat)
        .map(_.asInstanceOf[XmlFloat])
    }

  implicit val arbXmlDouble: Arbitrary[XmlDouble] =
    Arbitrary {
      Gen.double
        .map(XmlData.fromDouble)
        .map(_.asInstanceOf[XmlDouble])
    }

  implicit val arbXmlBigDecimal: Arbitrary[XmlBigDecimal] =
    Arbitrary {
      Arbitrary.arbBigDecimal.arbitrary
        .map(XmlData.fromBigDecimal)
        .map(_.asInstanceOf[XmlBigDecimal])
    }

  implicit val arbXmlXmlBigInt: Arbitrary[XmlBigInt] =
    Arbitrary {
      Arbitrary.arbBigInt.arbitrary
        .map(XmlData.fromBigInt)
        .map(_.asInstanceOf[XmlBigInt])
    }

  implicit val arbXmlData: Arbitrary[XmlData] =
    Arbitrary {
      Gen.oneOf[XmlData](
        arbXmlString.arbitrary,
        arbXmlChar.arbitrary,
        arbXmlBool.arbitrary,
        arbXmlInt.arbitrary,
        arbXmlLong.arbitrary,
        arbXmlFloat.arbitrary,
        arbXmlDouble.arbitrary,
        arbXmlBigDecimal.arbitrary,
        arbXmlXmlBigInt.arbitrary
      )
    }
}
