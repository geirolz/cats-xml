package cats.xml.testing.arbitrary

import cats.xml.codec.{Decoder, DecoderFailure, Encoder}
import cats.xml.cursor.CursorFailure
import cats.xml.XmlNode
import cats.xml.testing.Ops
import org.scalacheck.{Arbitrary, Cogen, Gen}

object CodecArbitrary {

  import cats.implicits.*
  import cats.xml.implicits.*

  implicit def arbEncoder[A](implicit
    agen: Arbitrary[A]
  ): Arbitrary[Encoder[A]] =
    Arbitrary {
      agen.arbitrary.map(a =>
        Encoder.pure(
          XmlNode(s"Node${a.hashCode()}").withAttrs(
            "Id"       := Ops.md5(a.toString),
            "hashcode" := a.hashCode()
          )
        )
      )
    }

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

  implicit val arbCustomCursorFailure: Arbitrary[CursorFailure] =
    Arbitrary {
      Gen.asciiPrintableStr.map(CursorFailure.Custom(_))
    }

  implicit val cogenCursorFailure: Cogen[CursorFailure] =
    Cogen.cogenString.contramap(_.toString)

  implicit val arbCustomDecodingFailure: Arbitrary[DecoderFailure] =
    Arbitrary {
      Gen.asciiPrintableStr.map(DecoderFailure.Custom(_))
    }

  implicit val cogenDecodingFailure: Cogen[DecoderFailure] =
    Cogen.cogenString.contramap(_.toString)
}
