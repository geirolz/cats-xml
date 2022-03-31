package cats.xml.codec

import cats.xml.Xml
import cats.xml.cursor.Cursor

/** Isomorphism
  */
case class Codec[T] private (
  decoder: Decoder[T],
  encoder: Encoder[T]
) {

  def decodeCursorResult(cursorResult: Cursor.Result[Xml]): Decoder.Result[T] =
    decoder.decodeCursorResult(cursorResult)

  def decode(xml: Xml): Decoder.Result[T] =
    decoder.decode(xml)

  def encode(t: T): Xml =
    encoder.encode(t)
}

object Codec {

  def apply[T: Codec]: Codec[T] =
    implicitly[Codec[T]]

  def of[T](decoder: Decoder[T], encoder: Encoder[T]): Codec[T] =
    new Codec[T](decoder, encoder)
}
