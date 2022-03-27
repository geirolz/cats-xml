package cats.xml.testing.codec

import cats.xml.codec.{Decoder, DecoderFailure}
import org.scalacheck.{Arbitrary, Cogen, Gen}

object arbitrary {

  import cats.implicits.*

  implicit def arbDecoder[A](implicit
    agen: Arbitrary[A],
    egen: Arbitrary[DecoderFailure]
  ): Arbitrary[Decoder[A]] =
    Arbitrary {
      Gen.oneOf(
        agen.arbitrary.map(a => Decoder.of(_ => a.validNel)),
        egen.arbitrary.map(e => Decoder.failure[A](e))
      )
    }

  implicit val arbCustomDecodingFailure: Arbitrary[DecoderFailure] =
    Arbitrary {
      Gen.asciiPrintableStr.map(DecoderFailure.Custom(_))
    }

  implicit val cogenDecodingFailure: Cogen[DecoderFailure] =
    Cogen.cogenString.contramap(_.toString)
}
