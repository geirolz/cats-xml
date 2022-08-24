package cats.xml

import cats.{xml, Show}
import cats.xml.Xml.XmlNull
import cats.xml.codec.Decoder

import scala.reflect.ClassTag

trait Xml {

  final lazy val isNull: Boolean = this match {
    case XmlNull => true
    case _       => false
  }

  final lazy val isData: Boolean =
    asData.isDefined

  final lazy val isAttribute: Boolean =
    asAttribute.isDefined

  final lazy val isNode: Boolean =
    asNode.isDefined

  final def asData: Option[XmlData] = this match {
    case value: XmlData => Some(value)
    case _              => None
  }

  final def asAttribute: Option[XmlAttribute] = this match {
    case value: XmlAttribute => Some(value)
    case _                   => None
  }

  final def asNode: Option[XmlNode] = this match {
    case value: XmlNode => Some(value)
    case _              => None
  }
}
object Xml {

  case object XmlNull extends Xml with XmlData

  final lazy val Null: Xml & XmlData = XmlNull
  final lazy val Data: XmlData.type  = xml.XmlData

  /*
   * https://www.w3.org/TR/REC-xml/#NT-NameChar
   * */
  def isValidXmlName(string: String): Boolean =
    string != null &&
      string.nonEmpty &&
      string.matches(
        "^[:A-Z_a-z\\u00C0\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02ff\\u0370-\\u037d"
          + "\\u037f-\\u1fff\\u200c\\u200d\\u2070-\\u218f\\u2c00-\\u2fef\\u3001-\\ud7ff"
          + "\\uf900-\\ufdcf\\ufdf0-\\ufffd\\x10000-\\xEFFFF]"
          + "[:A-Z_a-z\\u00C0\\u00D6\\u00D8-\\u00F6"
          + "\\u00F8-\\u02ff\\u0370-\\u037d\\u037f-\\u1fff\\u200c\\u200d\\u2070-\\u218f"
          + "\\u2c00-\\u2fef\\u3001-\\udfff\\uf900-\\ufdcf\\ufdf0-\\ufffd\\-\\.0-9"
          + "\\u00b7\\u0300-\\u036f\\u203f-\\u2040]*\\Z"
      )

  def requireValidXmlName(value: String): Unit =
    require(isValidXmlName(value), s"Invalid XML name [$value].")
}

sealed trait XmlData extends Xml with Serializable {

  def as[T: Decoder]: Decoder.Result[T] = Decoder[T].decode(this)

  def asString: String = Show[XmlData].show(this)

  def isEmpty: Boolean = this match {
    case XmlData.XmlString(value) => value.isEmpty
    case XmlData.XmlArray(value)  => value.isEmpty
    case _                        => false
  }

  override def toString: String = asString
}
object XmlData {

  lazy val True: XmlData  = fromBoolean(true)
  lazy val False: XmlData = fromBoolean(false)
  lazy val empty: XmlData = fromString("")

  def fromString(value: String): XmlData                      = XmlString(value)
  def fromNumber[N <: Number](value: N): XmlData              = XmlNumber(value)
  def fromArray[T <: XmlData](value: Array[T]): XmlData       = XmlArray(value)
  def fromSeq[T <: XmlData: ClassTag](value: Seq[T]): XmlData = XmlArray(value.toArray)
  def fromValues[T <: XmlData: ClassTag](value: T*): XmlData  = XmlArray(value.toArray)
  def fromByte(value: Byte): XmlData                          = XmlByte(value)
  def fromBoolean(value: Boolean): XmlData                    = XmlBool(value)

  private[xml] final case class XmlString(value: String) extends XmlData
  private[xml] final case class XmlNumber[T <: Number](value: T) extends XmlData
  private[xml] final case class XmlArray[T <: XmlData](value: Array[T]) extends XmlData
  private[xml] final case class XmlByte(value: Byte) extends XmlData
  private[xml] final case class XmlBool(value: Boolean) extends XmlData

  // TODO: TO CHECK EQ TO DECODE STRING
  implicit val showXmlData: Show[XmlData] = {
    case XmlNull          => ""
    case XmlByte(value)   => value.toString
    case XmlNumber(value) => value.toString
    case XmlArray(value)  => value.mkString(",")
    case XmlBool(value)   => value.toString
    case XmlString(value) => value
  }
}
