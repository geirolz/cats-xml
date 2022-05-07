package cats.xml.generic

sealed trait XmlElemType
object XmlElemType {
  case object Attribute extends XmlElemType
  case object Child extends XmlElemType
  case object Text extends XmlElemType
  case object Null extends XmlElemType
}
