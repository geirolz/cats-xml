package cats.xml.utils.generic

import scala.reflect.macros.blackbox

trait TypeInfoInstances {
  implicit def deriveTypeInfo[T]: TypeInfo[T] =
    macro TypeInfoMacros.deriveTypeInfoImpl[T]

  implicit def deriveFieldsTypeInfo[T]: Map[ParamName[T], TypeInfo[?]] =
    macro TypeInfoMacros.deriveFieldsTypeInfoImpl[T]
}
object TypeInfoMacros {

  def deriveTypeInfoImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[TypeInfo[T]] = {
    import c.universe.*

    val wtpe: c.universe.Type = weakTypeOf[T].finalResultType

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

    val wtpe = weakTypeOf[T].finalResultType

    val tuples: List[Tree] = wtpe.members.collect {
      case mSymbol: MethodSymbol if mSymbol.isGetter && mSymbol.isPublic =>
        val name = mSymbol.name.toString
        q"""
           import cats.xml.utils.generic.TypeInfo
           import cats.xml.utils.generic.* 
           (ParamName[${wtpe.typeSymbol}]($name), TypeInfo.deriveTypeInfo[${mSymbol.returnType.resultType.typeSymbol}])
         """
    }.toList

    c.Expr[Map[ParamName[T], TypeInfo[?]]](
      q"""List(..$tuples).toMap"""
    )
  }
}
