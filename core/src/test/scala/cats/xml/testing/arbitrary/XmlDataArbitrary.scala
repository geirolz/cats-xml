package cats.xml.testing.arbitrary

import cats.xml.{Xml, XmlData}
import cats.xml.XmlData.*
import org.scalacheck.{Arbitrary, Gen}

object XmlDataArbitrary {

  implicit val arbXmlString: Arbitrary[XmlString] =
    Arbitrary {
      Gen.asciiPrintableStr.map(Xml.string)
    }

  implicit val arbXmlChar: Arbitrary[XmlChar] =
    Arbitrary {
      Gen.alphaNumChar.map(Xml.char)
    }

  implicit val arbXmlBool: Arbitrary[XmlBool] =
    Arbitrary {
      Arbitrary.arbBool.arbitrary.map(Xml.boolean)
    }

//  implicit val arbXmlByte: Arbitrary[XmlByte] =
//    Arbitrary {
//      Arbitrary.arbByte.arbitrary
//        .map(Xml.fromByte)
//        .map(_.asInstanceOf[XmlByte])
//    }

//  implicit val arbXmlInt: Arbitrary[XmlInt] =
//    Arbitrary {
//      Arbitrary.arbInt.arbitrary
//        .map(Xml.fromInt)
//        .map(_.asInstanceOf[XmlInt])
//    }

  implicit val arbXmlLong: Arbitrary[XmlLong] =
    Arbitrary {
      Gen.long
        .map(Xml.long)
        .map(_.asInstanceOf[XmlLong])
    }

  implicit val arbXmlFloat: Arbitrary[XmlFloat] =
    Arbitrary {
      Arbitrary.arbFloat.arbitrary
        .map(Xml.float)
        .map(_.asInstanceOf[XmlFloat])
    }

  implicit val arbXmlDouble: Arbitrary[XmlDouble] =
    Arbitrary {
      Gen.double
        .map(Xml.double)
        .map(_.asInstanceOf[XmlDouble])
    }

  implicit val arbXmlBigDecimal: Arbitrary[XmlBigDecimal] =
    Arbitrary {
      Arbitrary.arbBigDecimal.arbitrary
        .map(Xml.bigDecimal)
        .map(_.asInstanceOf[XmlBigDecimal])
    }

//  implicit val arbXmlXmlBigInt: Arbitrary[XmlBigInt] =
//    Arbitrary {
//      Arbitrary.arbBigInt.arbitrary
//        .map(Xml.fromBigInt)
//        .map(_.asInstanceOf[XmlBigInt])
//    }

  implicit val arbXmlData: Arbitrary[XmlData] =
    Arbitrary {
      Gen.oneOf[XmlData](
        arbXmlString.arbitrary,
        arbXmlChar.arbitrary,
        arbXmlBool.arbitrary,
//        arbXmlInt.arbitrary,
        arbXmlLong.arbitrary,
        arbXmlFloat.arbitrary,
        arbXmlDouble.arbitrary,
        arbXmlBigDecimal.arbitrary
//        arbXmlXmlBigInt.arbitrary
      )
    }
}
