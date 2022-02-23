package cats.xml.codec

/** Isomorphism XML ~ T
  */
class Codec[T] private (
  // XML => T
  val decoder: Decoder[T],
  // T => XML
  val encoder: Encoder[T]
)

object Codec extends CodecInstances {

  def apply[T: Codec]: Codec[T] =
    implicitly[Codec[T]]

  def of[T](decoder: Decoder[T], encoder: Encoder[T]): Codec[T] =
    new Codec[T](decoder, encoder)
}

private[xml] trait CodecInstances {
  implicit def codecToDecoder[T: Codec]: Decoder[T] = Codec[T].decoder
  implicit def codecToEncoder[T: Codec]: Encoder[T] = Codec[T].encoder
}
