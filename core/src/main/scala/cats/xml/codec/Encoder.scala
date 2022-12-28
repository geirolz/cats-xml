package cats.xml.codec

import cats.xml.{Xml, XmlData}
import cats.Contravariant
import cats.data.Validated

// T => XML
trait Encoder[T] {

  def encode(t: T): Xml

  def contramap[U](f: U => T): Encoder[U] =
    Encoder.of(f.andThen(encode))
}
object Encoder extends EncoderInstances with EncoderSyntax {

  // lazy due circular dependency with instances
  lazy val id: Encoder[Xml] = Encoder.of(identity)

  def apply[T: Encoder]: Encoder[T] = implicitly[Encoder[T]]

  def of[T](f: T => Xml): Encoder[T] = (t: T) => f(t)

  def pure[T](ns: => Xml): Encoder[T] = Encoder(_ => ns)
}

// #################### SYNTAX ####################
private[xml] trait EncoderSyntax {

  implicit class EncoderOps[T](t: T) {

    def toXml(implicit e: Encoder[T]): Xml =
      e.encode(t)

    def toXmlWiden[TT >: T](implicit e: Encoder[TT]): Xml =
      e.encode(t)
  }
}

// #################### INSTANCES ####################
private[xml] trait EncoderInstances extends EncoderPrimitivesInstances {

  implicit def codecToEncoder[T: Codec]: Encoder[T] = Codec[T].encoder

  implicit val catsContravariantInstanceForEncoder: Contravariant[Encoder] =
    new Contravariant[Encoder] {
      override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = fa.contramap(f)
    }
}
private[xml] trait EncoderPrimitivesInstances {

  implicit val encoderXml: Encoder[Xml] = Encoder.id

  implicit def encoderNoneOption: Encoder[None.type] =
    Encoder.of(_ => Xml.Null)

  implicit def encodeOption[F[X] <: Option[X], T: Encoder]: Encoder[F[T]] =
    Encoder.of[F[T]](_.fold[Xml](Xml.Null)(Encoder[T].encode(_)))

  implicit def dataEncodeNoneOption: DataEncoder[None.type] =
    DataEncoder.of(_ => Xml.Null)

  implicit def dataEncodeOption[F[X] <: Option[X], T: DataEncoder]: DataEncoder[F[T]] =
    DataEncoder.of[F[T]](_.fold[XmlData](Xml.Null)(DataEncoder[T].encode(_)))

  implicit val encodeXmlData: DataEncoder[XmlData]       = DataEncoder.of(identity)
  implicit val encodeUnit: DataEncoder[Unit]             = DataEncoder.of(_ => Xml.Null)
  implicit val encodeString: DataEncoder[String]         = DataEncoder.of(XmlData.fromString(_))
  implicit val encodeChar: DataEncoder[Char]             = DataEncoder.of(XmlData.fromChar)
  implicit val encodeBoolean: DataEncoder[Boolean]       = DataEncoder.of(XmlData.fromBoolean)
  implicit val encodeInt: DataEncoder[Int]               = DataEncoder.of(XmlData.fromInt)
  implicit val encodeLong: DataEncoder[Long]             = DataEncoder.of(XmlData.fromLong)
  implicit val encodeFloat: DataEncoder[Float]           = DataEncoder.of(XmlData.fromFloat)
  implicit val encodeDouble: DataEncoder[Double]         = DataEncoder.of(XmlData.fromDouble)
  implicit val encodeBigDecimal: DataEncoder[BigDecimal] = DataEncoder.of(XmlData.fromBigDecimal)
  implicit val encodeBigInt: DataEncoder[BigInt]         = DataEncoder.of(XmlData.fromBigInt)

}

// #################### DATA ENCODER ####################
trait DataEncoder[T] extends Encoder[T] {
  override def encode(t: T): XmlData
  override def contramap[U](f: U => T): DataEncoder[U] =
    DataEncoder.of(f.andThen(encode))
}
object DataEncoder {

  def apply[T: DataEncoder]: DataEncoder[T] = implicitly[DataEncoder[T]]

  def of[T](f: T => XmlData): DataEncoder[T] = (t: T) => f(t)

  def stringParsedEncoder: DataEncoder[String] = {

    def fromValue[T](value: T): Either[DecoderFailure, XmlData] =
      value match {
        case v: String     => Right(XmlData.fromString(v))
        case v: Char       => Right(XmlData.fromChar(v))
        case v: Boolean    => Right(XmlData.fromBoolean(v))
        case v: Int        => Right(XmlData.fromInt(v))
        case v: Long       => Right(XmlData.fromLong(v))
        case v: Float      => Right(XmlData.fromFloat(v))
        case v: Double     => Right(XmlData.fromDouble(v))
        case v: BigDecimal => Right(XmlData.fromBigDecimal(v))
        case v: BigInt     => Right(XmlData.fromBigInt(v))
        case _             => Left(DecoderFailure.Custom("Cannot decode specified type."))
      }

    DataEncoder.of[String](strValue => {

      val res = Decoder
        .oneOf(
          Decoder.decodeBoolean.emap(fromValue),
          Decoder.decodeInt.emap(fromValue),
          Decoder.decodeLong.emap(fromValue),
          Decoder.decodeFloat.emap(fromValue),
          Decoder.decodeDouble.emap(fromValue),
          Decoder.decodeBigInt.emap(fromValue),
          Decoder.decodeBigDecimal.emap(fromValue),
          Decoder.decodeCharArray.emap(fromValue),
          Decoder.decodeString.emap(fromValue)
        )
        .decode(XmlData.fromString(strValue))

      res match {
        case Validated.Valid(a)   => a
        case Validated.Invalid(_) => XmlData.fromString(strValue)
      }
    })
  }
}
