package cats.xml.codec

import cats.xml.{Xml, XmlData, XmlString}
import cats.Contravariant

// T => XML
trait Encoder[-T] {

  def encode(t: T): Xml

  def contramap[U](f: U => T): Encoder[U] =
    Encoder.of(f.andThen(encode))
}
object Encoder extends EncoderInstances {

  // lazy due circular dependencies with instances
  lazy val id: Encoder[Xml] = Encoder.of(identity)

  def apply[T: Encoder]: Encoder[T] = implicitly[Encoder[T]]

  def of[T](f: T => Xml): Encoder[T] = (t: T) => f(t)

  def pure[T](ns: => Xml): Encoder[T] = Encoder(_ => ns)
}

private[xml] trait EncoderInstances extends EncoderPrimitivesInstances {

  implicit val catsContravariantInstanceForEncoder: Contravariant[Encoder] =
    new Contravariant[Encoder] {
      override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = fa.contramap(f)
    }
}
private[xml] trait EncoderPrimitivesInstances {
  implicit val encoderXml: Encoder[Xml] = Encoder.id
}

// data encoder
trait DataEncoder[-T] extends Encoder[T] {
  override def encode(t: T): XmlData
  override def contramap[U](f: U => T): DataEncoder[U] =
    DataEncoder.of(f.andThen(encode))
}
object DataEncoder extends DataEncoderPrimitivesInstances {
  def apply[T: DataEncoder]: DataEncoder[T] = implicitly[DataEncoder[T]]

  def of[T](f: T => XmlData): DataEncoder[T] = (t: T) => f(t)
}

private[xml] trait DataEncoderPrimitivesInstances {
  implicit val encoderString: DataEncoder[String] = DataEncoder.of(XmlString(_))
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
}
