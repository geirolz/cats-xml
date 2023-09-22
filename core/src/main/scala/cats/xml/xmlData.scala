package cats.xml

import cats.xml.codec.Decoder
import cats.{Eq, Order, Show}

import java.math.BigDecimal as JavaBigDecimal
import scala.util.Try

/** Represents a data value in XML.
  *
  * To instantiate a value of this type, use the `Xml.from*` methods.
  */
sealed trait XmlData extends Xml with Serializable {

  def as[T: Decoder]: Decoder.Result[T] = Decoder[T].decode(this)

  def asString: String = as[String].getOrElse("")

  def widen: XmlData = this

  def isEmpty: Boolean = this match {
    case XmlData.XmlString(value) => value.isEmpty
    case XmlData.XmlArray(value)  => value.isEmpty
    case _                        => false
  }
}
case object XmlNull extends Xml with XmlNode.Null with XmlData {
  override final def isEmpty: Boolean          = true
  // To avoid stackoverflow since the XmlPrinter used by the Eq instance uses patter matching.
  override final def equals(obj: Any): Boolean = obj.isInstanceOf[XmlNull.type]
}

object XmlData {

  import cats.syntax.all.*

  final case class XmlString(value: String) extends XmlData
  object XmlString {
    private[xml] def apply(value: String): XmlString = new XmlString(value)
  }
  final case class XmlChar(value: Char) extends XmlData
  object XmlChar {
    private[xml] def apply(value: Char): XmlChar = new XmlChar(value)
  }
  final case class XmlBool(value: Boolean) extends XmlData
  object XmlBool {
    private[xml] def apply(value: Boolean): XmlBool = new XmlBool(value)
  }
  final case class XmlArray[T <: XmlData](value: Array[T]) extends XmlData
  object XmlArray {
    private[xml] def apply[T <: XmlData](value: Array[T]): XmlArray[T] = new XmlArray(value)
  }

  /* Inspired by Circe library
   * https://github.com/circe/circe
   */
  sealed trait XmlNumber extends XmlData with Serializable {

    final def toBigDecimal: Option[BigDecimal] = this match {
      case XmlLong(value)       => Some(BigDecimal(value))
      case XmlFloat(value)      => Some(new JavaBigDecimal(java.lang.Float.toString(value)))
      case XmlDouble(value)     => Some(JavaBigDecimal.valueOf(value))
      case XmlBigDecimal(value) => Some(value)
    }

    final def toBigInt: Option[BigInt] = this match {
      case XmlLong(value) => Some(BigInt(value))
      case XmlFloat(value) =>
        Option(new JavaBigDecimal(java.lang.Float.toString(value)))
          .filter(XmlNumber.bigDecimalIsWhole)
          .map(bd => new BigInt(bd.toBigInteger))
      case XmlDouble(value) =>
        Option(JavaBigDecimal.valueOf(value))
          .filter(XmlNumber.bigDecimalIsWhole)
          .map(bd => new BigInt(bd.toBigInteger))
      case XmlBigDecimal(value) => value.toBigIntExact
    }

    final def toDouble: Double = this match {
      case XmlLong(value)       => value.toDouble
      case XmlFloat(value)      => new JavaBigDecimal(java.lang.Float.toString(value)).doubleValue
      case XmlDouble(value)     => value
      case XmlBigDecimal(value) => value.doubleValue
    }

    final def toFloat: Float = this match {
      case XmlLong(value)       => value.toFloat
      case XmlFloat(value)      => value
      case XmlDouble(value)     => value.toFloat
      case XmlBigDecimal(value) => value.floatValue
    }

    final def toByte: Option[Byte] = toLong match {
      case Some(n) =>
        val asByte: Byte = n.toByte
        if (n == asByte) Some(asByte) else None
      case None => None
    }

    final def toShort: Option[Short] = toLong match {
      case Some(n) =>
        val asShort: Short = n.toShort
        if (n == asShort) Some(asShort) else None
      case None => None
    }

    final def toInt: Option[Int] = toLong match {
      case Some(n) =>
        val asInt: Int = n.toInt
        if (n == asInt) Some(asInt) else None
      case None => None
    }

    final def toLong: Option[Long] = this match {
      case XmlLong(value) => Some(value)
      case XmlFloat(value) =>
        Option(new JavaBigDecimal(java.lang.Float.toString(value)))
          .filter(XmlNumber.bigDecimalIsValidLong)
          .map(_.longValue)
      case XmlDouble(value) =>
        Option(JavaBigDecimal.valueOf(value))
          .filter(XmlNumber.bigDecimalIsValidLong)
          .map(_.longValue)
      case XmlBigDecimal(value) => Try(value.toLongExact).toOption
    }

    final def stringValue: String = this match {
      case XmlLong(value)       => value.toString
      case XmlFloat(value)      => value.toString
      case XmlDouble(value)     => value.toString
      case XmlBigDecimal(value) => value.toString
    }
  }
  object XmlNumber {

    private[this] val bigDecimalMinLong: JavaBigDecimal = new JavaBigDecimal(Long.MinValue)
    private[this] val bigDecimalMaxLong: JavaBigDecimal = new JavaBigDecimal(Long.MaxValue)

    private[xml] def bigDecimalIsWhole(value: JavaBigDecimal): Boolean =
      value.signum == 0 || value.scale <= 0 || value.stripTrailingZeros.scale <= 0

    private[xml] def bigDecimalIsValidLong(value: JavaBigDecimal): Boolean =
      bigDecimalIsWhole(value) && value.compareTo(bigDecimalMinLong) >= 0 && value.compareTo(
        bigDecimalMaxLong
      ) <= 0
  }

  private[xml] final case class XmlLong(value: Long) extends XmlNumber
  private[xml] final case class XmlFloat(value: Float) extends XmlNumber
  private[xml] final case class XmlDouble(value: Double) extends XmlNumber
  private[xml] final case class XmlBigDecimal(value: BigDecimal) extends XmlNumber

  // ------------------------------------//

  implicit def showXmlData[T <: XmlData](implicit
    printer: XmlPrinter,
    config: XmlPrinter.Config
  ): Show[T] =
    printer.prettyString(_)

  implicit val order: Order[XmlNumber] = {
    Order.from {
      case (XmlLong(x), XmlLong(y))             => x.compareTo(y)
      case (XmlDouble(x), XmlDouble(y))         => java.lang.Double.compare(x, y)
      case (XmlFloat(x), XmlFloat(y))           => java.lang.Float.compare(x, y)
      case (XmlBigDecimal(x), XmlBigDecimal(y)) => x.compareTo(y)
      case (a, b) => (a.toBigDecimal, b.toBigDecimal).mapN(_.compareTo(_)).getOrElse(0)
    }
  }

  implicit val eqXmlData: Eq[XmlData] =
    (a: XmlData, b: XmlData) => a.as[String] == b.as[String]
}
