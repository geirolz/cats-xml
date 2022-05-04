package cats.xml.generic

import scala.reflect.runtime.universe.*

object Utils {

  def isValueClass(tpe: Type): Boolean =
    tpe <:< typeOf[AnyVal]

  def isValueClassOf(tpe: Type)(f: Type => Boolean): Boolean =
    isValueClass(tpe) && f(classAccessors(tpe).values.head)

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

  def isPrimitive[T: TypeTag](tpe: Type): Boolean =
    tpe.typeSymbol.asClass.isPrimitive

  def classAccessors[T: TypeTag]: Map[String, Type] =
    classAccessors(typeOf[T])

  def classAccessors(tpe: Type): Map[String, Type] =
    tpe.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => (m.name.toTermName.toString, m.returnType)
    }.toMap
}
