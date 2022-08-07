package cats.xml.utils.generic

import cats.Show

import scala.reflect.macros.blackbox

case class TypeInfo[T] private (
  isString: Boolean,
  isPrimitiveWrapper: Boolean,
  isPrimitive: Boolean,
  hasArgsTypePrimitive: Boolean,
  hasArgsTypeOfString: Boolean,
  isValueClass: Boolean,
  isValueClassOfPrimitivesOrString: Boolean,
  accessorsInfo: Map[ParamName[T], TypeInfo[?]]
) {
  override def toString: String = Show[TypeInfo[T]].show(this)
}
object TypeInfo {

  def apply[T: TypeInfo]: TypeInfo[T] = implicitly[TypeInfo[T]]

  def of[T](
    isString: Boolean,
    isPrimitiveWrapper: Boolean,
    isPrimitive: Boolean,
    hasArgsTypePrimitive: Boolean,
    hasArgsTypeOfString: Boolean,
    isValueClass: Boolean,
    isValueClassOfPrimitivesOrString: Boolean,
    accessorsInfo: Map[ParamName[T], TypeInfo[?]]
  ): TypeInfo[T] = new TypeInfo[T](
    isString,
    isPrimitiveWrapper,
    isPrimitive,
    hasArgsTypePrimitive,
    hasArgsTypeOfString,
    isValueClass,
    isValueClassOfPrimitivesOrString,
    accessorsInfo
  )

  object auto {
    implicit def deriveTypeInfo[T]: TypeInfo[T] =
      macro TypeInfoMacros.deriveTypeInfoImpl[T]

    implicit def deriveFieldsTypeInfo[T]: Map[ParamName[T], TypeInfo[?]] =
      macro TypeInfoMacros.deriveFieldsTypeInfoImpl[T]
  }

  implicit def showTypeInfo[T]: Show[TypeInfo[T]] =
    (t: TypeInfo[T]) => s"""
       |isString:  ${t.isString}
       |isPrimitive: ${t.isPrimitive}
       |hasArgsTypePrimitive: ${t.hasArgsTypePrimitive}
       |hasArgsTypeOfString: ${t.hasArgsTypeOfString}
       |isValueClass: ${t.isValueClass}
       |isValueClassOfPrimitivesOrString: ${t.isValueClassOfPrimitivesOrString}
       |accessorsInfo: ${t.accessorsInfo}
       |""".stripMargin
}

object TypeInfoMacros {

  def deriveTypeInfoImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[TypeInfo[T]] = {
    import c.universe.*

    val wtpe = weakTypeOf[T]

    // primitive
    def isPrimitive(tpe: c.universe.Type) =
      tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isPrimitive

    def isPrimitiveWrapper(tpe: c.universe.Type) =
      List(
        weakTypeOf[BigDecimal],
        weakTypeOf[BigInt]
      ).exists(pWrapperTpe => tpe <:< pWrapperTpe)

    // value class
    def isValueClass(tpe: c.universe.Type): Boolean =
      tpe <:< typeOf[AnyVal] &&
        tpe.typeSymbol.isClass &&
        tpe.typeSymbol.asClass.isCaseClass &&
        getAccessors(tpe).size == 1

    def isValueClassOfPrimitivesOrString(tpe: c.universe.Type): Boolean = {
      isValueClass(tpe)
      && getAccessors(tpe).headOption.exists(ptpe => {
        isPrimitive(ptpe.info) || isPrimitiveWrapper(ptpe.info)
      })
    }

    // utils
    def getAccessors(tpe: c.universe.Type): Iterable[c.universe.MethodSymbol] =
      tpe.members.collect {
        case m: MethodSymbol if m.isGetter && m.isPublic => m
      }

    c.Expr[TypeInfo[T]](
      q"""
          import cats.xml.utils.generic.TypeInfo
          import cats.xml.utils.generic.*
          import scala.reflect.runtime.universe.*

          TypeInfo.of[${wtpe.typeSymbol}](
            isString                         = ${wtpe <:< weakTypeOf[String]},
            isPrimitiveWrapper               = ${isPrimitiveWrapper(wtpe)},
            isPrimitive                      = ${isPrimitive(wtpe)},
            hasArgsTypePrimitive             = false,
            hasArgsTypeOfString              = false,
            isValueClass                     = ${isValueClass(wtpe)},
            isValueClassOfPrimitivesOrString = ${isValueClassOfPrimitivesOrString(wtpe)},
            accessorsInfo                    = ${deriveFieldsTypeInfoImpl[T](c)}
          )
         """
    )
  }

  def deriveFieldsTypeInfoImpl[T: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[Map[ParamName[T], TypeInfo[?]]] = {
    import c.universe.*

    val wtpe = weakTypeOf[T]
    val tuples: List[Tree] = wtpe.members.collect {
      case mSymbol: MethodSymbol if mSymbol.isGetter && mSymbol.isPublic =>
        val name = mSymbol.name.toString
        q"""
           import cats.xml.utils.generic.TypeInfo
           import cats.xml.utils.generic.* 
           (ParamName[${wtpe.typeSymbol}]($name), TypeInfo.auto.deriveTypeInfo[${mSymbol.returnType.typeSymbol}])
         """
    }.toList

    c.Expr[Map[ParamName[T], TypeInfo[?]]](
      q"""List(..$tuples).toMap"""
    )
  }

}
