package cats.xml.utils.generic

import cats.Show

case class TypeInfo[T](
  isString: Boolean,
  isPrimitiveWrapper: Boolean,
  isPrimitive: Boolean,
  isValueClass: Boolean,
  isOptionOfAnyPrimitiveOrString: Boolean,
  accessorsInfo: Map[ParamName[T], TypeInfo[?]],
  isNonPrimitiveValueClass: Boolean
) {
  override def toString: String = Show[TypeInfo[T]].show(this)
}
object TypeInfo extends TypeInfoInstances {

  def apply[T: TypeInfo]: TypeInfo[T] = implicitly[TypeInfo[T]]

  private def apply[T](
    isString: Boolean,
    isPrimitiveWrapper: Boolean,
    isPrimitive: Boolean,
    isValueClass: Boolean,
    isOptionOfAnyPrimitiveOrString: Boolean,
    accessorsInfo: Map[ParamName[T], TypeInfo[?]],
    isNonPrimitiveValueClass: Boolean
  ): TypeInfo[T] = new TypeInfo[T](
    isString,
    isPrimitiveWrapper,
    isPrimitive,
    isValueClass,
    isOptionOfAnyPrimitiveOrString,
    accessorsInfo,
    isNonPrimitiveValueClass
  )

  def of[T](
    isString: Boolean,
    isPrimitiveWrapper: Boolean,
    isPrimitive: Boolean,
    isValueClass: Boolean,
    isOptionOfAnyPrimitiveOrString: Boolean,
    accessorsInfo: Map[ParamName[T], TypeInfo[?]],
    isNonPrimitiveValueClass: Boolean
  ): TypeInfo[T] = TypeInfo[T](
    isString,
    isPrimitiveWrapper,
    isPrimitive,
    isValueClass,
    isOptionOfAnyPrimitiveOrString,
    accessorsInfo,
    isNonPrimitiveValueClass
  )

  implicit def showTypeInfo[T]: Show[TypeInfo[T]] =
    (t: TypeInfo[T]) =>
      s"""
       |isString:  ${t.isString}
       |isPrimitiveWrapper: ${t.isPrimitive}
       |isPrimitive: ${t.isPrimitive}
       |isValueClass: ${t.isValueClass}
       |isOptionOfAnyPrimitiveOrString: ${t.isValueClass}
       |accessorsInfo: ${t.accessorsInfo}
       |isNonPrimitiveValueClass: ${t.isNonPrimitiveValueClass}""".stripMargin
}
