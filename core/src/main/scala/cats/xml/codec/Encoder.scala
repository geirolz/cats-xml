package cats.xml.codec

import cats.xml.{Xml, XmlData}
import cats.Contravariant

// T => XML
trait Encoder[-T] {

  def encode(t: T): Xml

  def contramap[U](f: U => T): Encoder[U] =
    Encoder.of(f.andThen(encode))
}
object Encoder extends EncoderInstances with EncoderSyntax {

  // lazy due circular dependencies with instances
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

  implicit def encoderOption[T: Encoder]: Encoder[Option[T]] =
    Encoder.of[Option[T]] {
      case Some(value) => Encoder[T].encode(value)
      case None        => Xml.Null
    }

  implicit val encoderXmlData: DataEncoder[XmlData] = DataEncoder.of(identity)
  implicit val encoderUnit: DataEncoder[Unit]       = DataEncoder.of(_ => Xml.Null)
  implicit val encoderString: DataEncoder[String]   = DataEncoder.of(Xml.Data.fromString(_))
  implicit val encoderBoolean: DataEncoder[Boolean] = encoderString.contramap {
    case true  => "true"
    case false => "false"
  }
  implicit val encoderChar: DataEncoder[Char]             = encoderString.contramap(_.toString)
  implicit val encoderInt: DataEncoder[Int]               = encoderString.contramap(_.toString)
  implicit val encoderLong: DataEncoder[Long]             = encoderString.contramap(_.toString)
  implicit val encoderFloat: DataEncoder[Float]           = encoderString.contramap(_.toString)
  implicit val encoderDouble: DataEncoder[Double]         = encoderString.contramap(_.toString)
  implicit val encoderBigDecimal: DataEncoder[BigDecimal] = encoderString.contramap(_.toString)
  implicit val encoderBigInt: DataEncoder[BigInt]         = encoderString.contramap(_.toString)
}

// #################### DATA ENCODER ####################
trait DataEncoder[-T] extends Encoder[T] {
  override def encode(t: T): XmlData
  override def contramap[U](f: U => T): DataEncoder[U] =
    DataEncoder.of(f.andThen(encode))
}
object DataEncoder {
  def apply[T: DataEncoder]: DataEncoder[T] = implicitly[DataEncoder[T]]

  def of[T](f: T => XmlData): DataEncoder[T] = (t: T) => f(t)
}
