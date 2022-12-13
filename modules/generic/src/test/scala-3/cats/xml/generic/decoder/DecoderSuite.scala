//package cats.xml.generic.decoder
//
//import cats.data.Validated.Valid
//import cats.xml.XmlNode
//import cats.xml.codec.Decoder
//import cats.xml.generic.{Samples, XmlElemType, XmlTypeInterpreter}
//
//class DecoderSuite extends munit.FunSuite {
//
//  import cats.xml.syntax.given
//  import Samples.*
//
//  test("auto") {
//
//    given XmlTypeInterpreter[Foo] =
//      XmlTypeInterpreter
//        .default[Foo]
//        .overrideType(
//          _.param(_.valueClass) -> XmlElemType.Attribute
//        )
//
////    given Decoder[Bar] = deriveDecoder[Bar]
////    given Decoder[Foo] = deriveDecoder[Foo]
//
//    assertEquals(
//      obtained = XmlNode("foo")
//        .withAttributes(
//          "primitiveField" := 1d,
//          "valueClass"     := "TEST"
//        )
//        .withChildren(
//          XmlNode("bar")
//            .withAttributes(
//              "field1" := "BHO",
//              "field2" := BigDecimal(100)
//            )
//        )
//        .as[Foo],
//      expected = Valid(
//        Foo(
//          primitiveField = 1d,
//          valueClass     = ValueClass("TEST"),
//          bar            = Bar("BHO", BigDecimal(100)),
//          missingField   = None,
//          missingNode    = None
//        )
//      )
//    )
//  }
//}
