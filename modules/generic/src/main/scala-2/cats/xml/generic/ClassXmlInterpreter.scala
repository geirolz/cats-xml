package cats.xml.generic

import cats.xml.generic.ClassXmlInterpreter.XmlElemType

import scala.reflect.runtime.universe.{Type, TypeTag}

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

    val classFields: Map[String, Type] = Utils.classAccessors[T]

    override def evalParameter(paramName: String): Option[XmlElemType] =
      classFields
        .get(paramName)
        .map(tpe => {

          val isString: Boolean             = Utils.isClassOf[String](tpe)
          val isPrimitive: Boolean          = Utils.isPrimitive(tpe)
          val hasArgsTypePrimitive: Boolean = Utils.hasArgsTypePrimitive(tpe)
          val hasArgsTypeOfString: Boolean  = Utils.hasArgsTypeOf[String](tpe)
          val isValueClass: Boolean = Utils.isValueClassOf(tpe)(fieldType =>
            Utils.isClassOf[String](fieldType) || Utils.isPrimitive(tpe)
          )

          Console.println(s"""
              |Type: $tpe
              |isString: $isString
              |isPrimitive: $isPrimitive
              |hasArgsTypePrimitive: $hasArgsTypePrimitive
              |hasArgsTypeOfString: $hasArgsTypeOfString
              |isValueClass: $isValueClass
              |""".stripMargin)
          if (
            isString || isPrimitive || hasArgsTypePrimitive || hasArgsTypeOfString || isValueClass
          )
            XmlElemType.Attribute
          else
            XmlElemType.Child
        })
  }

  implicit def provideDefaultWithChildAndAttributes[T: TypeTag]: ClassXmlInterpreter[T] =
    ClassXmlInterpreter.withChildAndAttributes[T]
}
