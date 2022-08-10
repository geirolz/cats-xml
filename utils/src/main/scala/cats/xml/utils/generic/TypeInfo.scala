package cats.xml.utils.generic

import cats.Show

case class TypeInfo[T] private (
  isString: Boolean,
  isPrimitiveWrapper: Boolean,
  isPrimitive: Boolean,
  isValueClass: Boolean,
  accessorsInfo: Map[ParamName[T], TypeInfo[?]]
) {
  override def toString: String = Show[TypeInfo[T]].show(this)
}
object TypeInfo extends TypeInfoInstances {

  def apply[T: TypeInfo]: TypeInfo[T] = implicitly[TypeInfo[T]]

  def of[T](
    isString: Boolean,
    isPrimitiveWrapper: Boolean,
    isPrimitive: Boolean,
    isValueClass: Boolean,
    accessorsInfo: Map[ParamName[T], TypeInfo[?]]
  ): TypeInfo[T] = new TypeInfo[T](
    isString,
    isPrimitiveWrapper,
    isPrimitive,
    isValueClass,
    accessorsInfo
  )

  implicit def showTypeInfo[T]: Show[TypeInfo[T]] =
    (t: TypeInfo[T]) => s"""
       |isString:  ${t.isString}
       |isPrimitive: ${t.isPrimitive}
       |isValueClass: ${t.isValueClass}
       |accessorsInfo: ${t.accessorsInfo}""".stripMargin
}
