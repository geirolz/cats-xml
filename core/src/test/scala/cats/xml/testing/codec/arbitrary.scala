package cats.xml.testing.codec

import cats.xml.codec.{Decoder, DecodingFailure}
import org.scalacheck.{Arbitrary, Cogen, Gen}

object arbitrary {

  implicit def arbDecoder[A](implicit
    agen: Arbitrary[A],
    egen: Arbitrary[DecodingFailure]
  ): Arbitrary[Decoder[A]] =
    Arbitrary {
      Gen.oneOf(
        agen.arbitrary.map(a => Decoder.of(_ => Decoder.Result.success[A](a))),
        egen.arbitrary.map(e => Decoder.failed[A](e))
      )
    }

  implicit val arbCustomDecodingFailure: Arbitrary[DecodingFailure] =
    Arbitrary {
      Gen.asciiPrintableStr.map(DecodingFailure.custom)
    }

  implicit val cogenDecodingFailure: Cogen[DecodingFailure] =
    Cogen.cogenString.contramap(_.reason.toString)
}
