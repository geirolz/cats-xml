package cats.xml.generic

import cats.Show

import scala.reflect.runtime.universe.*

case class TypeInfo(
  tpe: Type,
  isString: Boolean,
  isPrimitive: Boolean,
  hasArgsTypePrimitive: Boolean,
  hasArgsTypeOfString: Boolean,
  isValueClassOfPrimitivesOrString: Boolean
) {

  override def toString: String = Show[TypeInfo].show(this)
}
object TypeInfo {
  def eval(tpe: Type): TypeInfo =
    TypeInfo(
      tpe                  = tpe,
      isString             = Utils.isClassOf[String](tpe),
      isPrimitive          = Utils.isPrimitive(tpe),
      hasArgsTypePrimitive = Utils.hasArgsTypePrimitive(tpe),
      hasArgsTypeOfString  = Utils.hasArgsTypeOf[String](tpe),
      isValueClassOfPrimitivesOrString = Utils.isValueClassOf(tpe)(fieldType =>
        Utils.isClassOf[String](fieldType) || Utils.isPrimitive(tpe)
      )
    )

  implicit val showTypeInfo: Show[TypeInfo] =
    (t: TypeInfo) => s"""
       |Type: ${t.tpe}
       |isString:  ${t.isString}
       |isPrimitive: ${t.isPrimitive}
       |hasArgsTypePrimitive: ${t.hasArgsTypePrimitive}
       |hasArgsTypeOfString: ${t.hasArgsTypeOfString}
       |isValueClassOfPrimitivesOrString: ${t.isValueClassOfPrimitivesOrString}
       |""".stripMargin
}
