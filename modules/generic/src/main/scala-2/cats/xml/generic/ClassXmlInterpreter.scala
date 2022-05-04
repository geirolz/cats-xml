package cats.xml.generic

import cats.xml.generic.ClassXmlInterpreter.XmlElemType

import scala.reflect.runtime.universe.TypeTag

trait ClassXmlInterpreter[T] {
  def evalParameter(paramName: String): Option[XmlElemType]
}
object ClassXmlInterpreter {

  sealed trait XmlElemType
  object XmlElemType {
    case object Attribute extends XmlElemType
    case object Text extends XmlElemType
    case object Child extends XmlElemType
  }

  def apply[T](implicit i: ClassXmlInterpreter[T]): ClassXmlInterpreter[T] = i

  def withChildAndAttributes[T: TypeTag]: ClassXmlInterpreter[T] = new ClassXmlInterpreter[T] {

    val classFieldsInfo: Map[String, TypeInfo] = Utils.classAccessors[T]

    override def evalParameter(paramName: String): Option[XmlElemType] =
      classFieldsInfo
        .get(paramName)
        .map(tpeInfo =>
          if (
            tpeInfo.isString
            || tpeInfo.isPrimitive
            || tpeInfo.hasArgsTypePrimitive
            || tpeInfo.hasArgsTypeOfString
            || tpeInfo.isValueClassOfPrimitivesOrString
          )
            XmlElemType.Attribute
          else
            XmlElemType.Child
        )
  }

  implicit def provideDefaultWithChildAndAttributes[T: TypeTag]: ClassXmlInterpreter[T] =
    ClassXmlInterpreter.withChildAndAttributes[T]
}
