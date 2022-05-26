package cats.xml.generic

import scala.reflect.runtime.universe.*

object Utils {

  def isValueClass(tpe: Type): Boolean =
    tpe <:< typeOf[AnyVal]

  def isValueClassOf(tpe: Type)(f: Type => Boolean): Boolean =
    isValueClass(tpe) && classAccessors(tpe).values.headOption.exists(a => f(a.tpe))

  def hasArgsTypePrimitive(tpe: Type): Boolean = {
    val typeArgs = tpe.typeArgs
    typeArgs.nonEmpty && tpe.typeArgs.forall(_.typeSymbol.asClass.isPrimitive)
  }

  def hasArgsTypeOf[T: TypeTag](tpe: Type): Boolean = {
    val typeArgs = tpe.typeArgs
    typeArgs.nonEmpty && tpe.typeArgs.forall(isClassOf[T](_))
  }

  def isClassOf[T: TypeTag](tpe: Type): Boolean =
    tpe.typeSymbol.asClass.equals(typeOf[T].typeSymbol.asClass)

  def isPrimitive(tpe: Type): Boolean =
    tpe.typeSymbol.asClass.isPrimitive

  def classAccessors[T: TypeTag]: Map[ParamName[T], TypeInfo] =
    classAccessors(typeOf[T])

  def classAccessors[T](tpe: Type): Map[ParamName[T], TypeInfo] =
    tpe.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic =>
        (ParamName[T](m.name.toTermName.toString), TypeInfo.eval(m.returnType))
    }.toMap
}
