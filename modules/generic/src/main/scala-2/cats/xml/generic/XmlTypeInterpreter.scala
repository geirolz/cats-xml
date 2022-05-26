package cats.xml.generic

import cats.Endo

import scala.reflect.runtime.universe.TypeTag

case class XmlElemTypeParamInfo(
  elemType: XmlElemType,
  labelMapper: Endo[String] = identity
)

abstract class XmlTypeInterpreter[T] { $this =>

  def evalParam(paramName: ParamName[T]): Option[XmlElemTypeParamInfo]

  def overrideType(
    override1: ParamNameExtractor[T] => (ParamName[T], XmlElemType),
    overrideN: (ParamNameExtractor[T] => (ParamName[T], XmlElemType))*
  ): XmlTypeInterpreter[T]

  def overrideParamName(
    override1: ParamNameExtractor[T] => (ParamName[T], Endo[String]),
    overrideN: (ParamNameExtractor[T] => (ParamName[T], Endo[String]))*
  ): XmlTypeInterpreter[T]
}
object XmlTypeInterpreter {

  import cats.implicits.*

  def apply[T](implicit i: XmlTypeInterpreter[T]): XmlTypeInterpreter[T] = i

  def fullOf[T: TypeTag](
    f: (ParamName[T], TypeInfo) => (XmlElemType, Endo[String])
  ): XmlTypeInterpreter[T] =
    new XmlTypeInterpreter[T] {

      val classFieldsInfo: Map[ParamName[T], TypeInfo] = Utils.classAccessors[T]
      val classInfoExtractor: ParamNameExtractor[T]    = ParamNameExtractor.of[T]

      override def evalParam(paramName: ParamName[T]): Option[XmlElemTypeParamInfo] =
        classFieldsInfo
          .get(paramName)
          .map { tpeInfo =>
            val (elemType, labelCase) = f(paramName, tpeInfo)
            XmlElemTypeParamInfo(
              elemType    = elemType,
              labelMapper = labelCase
            )
          }

      def overrideType(
        override1: ParamNameExtractor[T] => (ParamName[T], XmlElemType),
        overrideN: (ParamNameExtractor[T] => (ParamName[T], XmlElemType))*
      ): XmlTypeInterpreter[T] =
        XmlTypeInterpreter.fullOf[T]((label, tpe) => {

          val mapping: Map[ParamName[T], XmlElemType] =
            (Seq(override1) ++ overrideN)
              .map(_.apply(classInfoExtractor))
              .toMap

          f.tupled((label, tpe))
            .leftMap(current => mapping.getOrElse(label, current))
        })

      def overrideParamName(
        override1: ParamNameExtractor[T] => (ParamName[T], Endo[String]),
        overrideN: (ParamNameExtractor[T] => (ParamName[T], Endo[String]))*
      ): XmlTypeInterpreter[T] =
        XmlTypeInterpreter.fullOf[T]((label, tpe) => {

          val mapping: Map[ParamName[T], Endo[String]] =
            (Seq(override1) ++ overrideN)
              .map(_.apply(classInfoExtractor))
              .toMap

          f.tupled((label, tpe))
            .map(current => mapping.getOrElse(label, current))
        })
    }

  def of[T: TypeTag](
    f: (ParamName[T], TypeInfo) => XmlElemType,
    labelMapper: Endo[String] = identity
  ): XmlTypeInterpreter[T] =
    XmlTypeInterpreter.fullOf[T]((label, tpe) => f.tupled.andThen(_ -> labelMapper)((label, tpe)))

  def auto[T: TypeTag](
    textDiscriminator: (ParamName[T], TypeInfo) => Boolean,
    attrsDiscriminator: (ParamName[T], TypeInfo) => Boolean =
      (_: ParamName[T], tpeInfo: TypeInfo) =>
        tpeInfo.isString
          || tpeInfo.isPrimitive
          || tpeInfo.isPrimitiveWrapper
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
    textField: ParamNameExtractor[T] => ParamName[T],
    otherTextFields: ParamNameExtractor[T] => ParamName[T]*
  ): XmlTypeInterpreter[T] =
    XmlTypeInterpreter.auto[T](
      textDiscriminator = (paramName, _) =>
        (Seq(textField) ++ otherTextFields)
          .map(_.apply(ParamNameExtractor.of[T]))
          .contains(paramName)
    )

  implicit def defaultWithoutText[T: TypeTag]: XmlTypeInterpreter[T] =
    XmlTypeInterpreter.withoutText[T]
}
