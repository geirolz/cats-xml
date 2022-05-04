package cats.xml.generic

import cats.Endo

import scala.reflect.runtime.universe.TypeTag

case class XmlElemTypeParamInfo(
  elemType: XmlElemType,
  labelMapper: Endo[String] = identity
)

abstract class XmlTypeInterpreter[T] { $this =>

  def evalParam(paramName: String): Option[XmlElemTypeParamInfo]

  def overrideType(overrider: PartialFunction[String, XmlElemType]): XmlTypeInterpreter[T]

  def overrideLabelMapper(overrider: PartialFunction[String, Endo[String]]): XmlTypeInterpreter[T]
}
object XmlTypeInterpreter {

  import cats.implicits.*

  def apply[T](implicit i: XmlTypeInterpreter[T]): XmlTypeInterpreter[T] = i

  def fullOf[T: TypeTag](
    f: (String, TypeInfo) => (XmlElemType, Endo[String])
  ): XmlTypeInterpreter[T] =
    new XmlTypeInterpreter[T] {

      val classFieldsInfo: Map[String, TypeInfo] = Utils.classAccessors[T]

      override def evalParam(paramName: String): Option[XmlElemTypeParamInfo] =
        classFieldsInfo
          .get(paramName)
          .map { tpeInfo =>
            val (elemType, labelCase) = f(paramName, tpeInfo)
            XmlElemTypeParamInfo(
              elemType    = elemType,
              labelMapper = labelCase
            )
          }

      override def overrideType(
        overrider: PartialFunction[String, XmlElemType]
      ): XmlTypeInterpreter[T] =
        XmlTypeInterpreter.fullOf((label, tpe) =>
          f.tupled((label, tpe)).leftMap(_ => overrider(label))
        )

      override def overrideLabelMapper(
        overrider: PartialFunction[String, Endo[String]]
      ): XmlTypeInterpreter[T] =
        XmlTypeInterpreter.fullOf((label, tpe) => f.tupled((label, tpe)).map(_ => overrider(label)))
    }

  def of[T: TypeTag](
    f: (String, TypeInfo) => XmlElemType,
    labelMapper: Endo[String] = identity
  ): XmlTypeInterpreter[T] =
    XmlTypeInterpreter.fullOf[T]((label, tpe) => f.tupled.andThen(_ -> labelMapper)((label, tpe)))

  def auto[T: TypeTag](
    textDiscriminator: (String, TypeInfo) => Boolean,
    attrsDiscriminator: (String, TypeInfo) => Boolean = (_, tpeInfo) =>
      tpeInfo.isString
        || tpeInfo.isPrimitive
        || tpeInfo.hasArgsTypePrimitive
        || tpeInfo.hasArgsTypeOfString
        || tpeInfo.isValueClassOfPrimitivesOrString
  ): XmlTypeInterpreter[T] =
    XmlTypeInterpreter.of[T] { case (paramName, tpeInfo) =>
      if (textDiscriminator(paramName, tpeInfo))
        XmlElemType.Text
      else if (attrsDiscriminator(paramName, tpeInfo))
        XmlElemType.Attribute
      else
        XmlElemType.Child
    }

  def withoutText[T: TypeTag]: XmlTypeInterpreter[T] =
    XmlTypeInterpreter.auto[T](textDiscriminator = (_, _) => false)

  def withTextFields[T: TypeTag](
    textField: String,
    otherTextFields: String*
  ): XmlTypeInterpreter[T] =
    XmlTypeInterpreter.auto[T](
      textDiscriminator = (paramName, _) => (Seq(textField) ++ otherTextFields).contains(paramName)
    )

  implicit def defaultWithoutText[T: TypeTag]: XmlTypeInterpreter[T] =
    XmlTypeInterpreter.withoutText[T]
}
