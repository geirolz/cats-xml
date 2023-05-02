package cats.xml.codec

import cats.xml.{Xml, XmlData}
import cats.Contravariant

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

  implicit def encodeXmlData[D <: XmlData]: DataEncoder[D] = DataEncoder.of(identity)
  implicit val encodeUnit: DataEncoder[Unit]               = DataEncoder.of(_ => Xml.Null)
  implicit val encodeString: DataEncoder[String]           = DataEncoder.of(Xml.ofString(_))
  implicit val encodeChar: DataEncoder[Char]               = DataEncoder.of(Xml.ofChar)
  implicit val encodeBoolean: DataEncoder[Boolean]         = DataEncoder.of(Xml.ofBoolean)
  implicit val encodeByte: DataEncoder[Byte]               = DataEncoder.of(Xml.ofByte)
  implicit val encodeShort: DataEncoder[Short]             = DataEncoder.of(Xml.ofShort)
  implicit val encodeInt: DataEncoder[Int]                 = DataEncoder.of(Xml.ofInt)
  implicit val encodeLong: DataEncoder[Long]               = DataEncoder.of(Xml.ofLong)
  implicit val encodeFloat: DataEncoder[Float]             = DataEncoder.of(Xml.ofFloat)
  implicit val encodeDouble: DataEncoder[Double]           = DataEncoder.of(Xml.ofDouble)
  implicit val encodeBigDecimal: DataEncoder[BigDecimal]   = DataEncoder.of(Xml.ofBigDecimal)
  implicit val encodeBigInt: DataEncoder[BigInt]           = DataEncoder.of(Xml.ofBigInt)
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
}
