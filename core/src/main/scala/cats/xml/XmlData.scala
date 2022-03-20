package cats.xml

import cats.Show
import cats.xml.codec.Decoder

sealed trait XmlData extends Xml with Serializable {

  def as[T: Decoder]: Decoder.Result[T] = Decoder[T].decode(this)

  lazy val isNull: Boolean = this match {
    case XmlNull => true
    case _       => false
  }

  override def toString: String = Show[XmlData].show(this)
}
object XmlData {

  // TODO: TO CHECK EQ TO DECODE STRING
  implicit val showXmlData: Show[XmlData] = {
    case XmlNull          => "null"
    case XmlByte(value)   => value.toString
    case XmlNumber(value) => value.toString
    case XmlArray(value)  => value.mkString(",")
    case XmlBool(value)   => value.toString
    case XmlString(value) => value
  }
}

case class XmlString(value: String) extends XmlData {
  def isEmpty: Boolean = value.isEmpty
}
object XmlString {
  val empty: XmlString = XmlString("")
}

case object XmlNull extends XmlData
case class XmlNumber[T <: Number](value: T) extends XmlData
case class XmlArray[T <: XmlData](value: Array[T]) extends XmlData
case class XmlByte(value: Byte) extends XmlData
case class XmlBool(value: Boolean) extends XmlData
