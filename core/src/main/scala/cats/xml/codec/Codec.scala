package cats.xml.codec

import cats.xml.Xml

/** Isomorphism
  */
case class Codec[T](
  decoder: Decoder[T],
  encoder: Encoder[T]
) {

  def decode(xml: Xml): Decoder.Result[T] =
    decoder.decode(xml)

  def encode(t: T): Xml =
    encoder.encode(t)
}

object Codec extends DecoderSyntax with EncoderSyntax {

  private def apply[T](decoder: Decoder[T], encoder: Encoder[T]): Codec[T] =
    new Codec(decoder, encoder)

  def apply[T: Codec]: Codec[T] =
    implicitly[Codec[T]]

  def of[T](decoder: Decoder[T], encoder: Encoder[T]): Codec[T] =
    Codec[T](decoder, encoder)

  implicit def codecToDecoder[T: Codec]: Decoder[T] =
    Codec[T].decoder

  implicit def codecToEncoder[T: Codec]: Encoder[T] =
    Codec[T].encoder
}
