package cats.xml.scalaxml

import cats.xml.{Xml, XmlAttribute}

import scala.annotation.unused
import scala.xml.{MetaData, Null}

private[scalaxml] object XmlAttributeConverter {

  def fromMetaData(metaData: MetaData): List[XmlAttribute] =
    metaData.iterator.map(m => XmlAttribute(m.key, Xml.string(m.value.text))).toList

  def toMetaData(attr: XmlAttribute): MetaData =
    new scala.xml.UnprefixedAttribute(attr.key, attr.value.asString, Null)
}

private[scalaxml] trait XmlAttributeConverterSyntax {

  implicit class MetaDataOps(metadata: MetaData) {
    def toXmlAttribute: List[XmlAttribute] =
      XmlAttributeConverter.fromMetaData(metadata)
  }

  implicit class XmlAttributeOps(attr: XmlAttribute) {
    def toMetaData: MetaData =
      XmlAttributeConverter.toMetaData(attr)
  }

  implicit class XmlAttributeObjOps(@unused obj: XmlAttribute.type) {

    def fromMetaData(metaData: MetaData): List[XmlAttribute] =
      XmlAttributeConverter.fromMetaData(metaData)

    def toMetaData(attr: XmlAttribute): MetaData =
      XmlAttributeConverter.toMetaData(attr)
  }
}
