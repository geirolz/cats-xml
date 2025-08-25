package cats.xml.utils.generic

import scala.reflect.macros.blackbox

trait TypeInfoInstances {
  implicit def deriveTypeInfo[T]: TypeInfo[T] =
    macro TypeInfoInstances.Scala2Macros.deriveTypeInfoImpl[T]

  implicit def deriveFieldsTypeInfo[T]: Map[ParamName[T], TypeInfo[?]] =
    macro TypeInfoInstances.Scala2Macros.deriveFieldsTypeInfoImpl[T]
}

object TypeInfoInstances {

  object Scala2Macros {

    def deriveTypeInfoImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[TypeInfo[T]] = {
      import c.universe.*

      val wtpe: c.universe.Type                   = weakTypeOf[T].finalResultType
      val utils: TypesUtils[c.type]               = new TypesUtils(c)
      val isString: Boolean                       = utils.isString(wtpe)
      val isPrimitiveWrapper: Boolean             = utils.isPrimitiveWrapper(wtpe)
      val isPrimitive: Boolean                    = utils.isPrimitive(wtpe)
      val isValueClass: Boolean                   = utils.isValueClass(wtpe)
      val isNonPrimitiveValueClass: Boolean       = utils.isNonPrimitiveValueClass(wtpe)
      val isOptionOfAnyPrimitiveOrString: Boolean = utils.isOptionOfAnyPrimitiveOrString(wtpe)
      val accessorsInfo: c.Expr[Map[ParamName[T], TypeInfo[?]]] = deriveFieldsTypeInfoImpl[T](c)

      c.Expr[TypeInfo[T]](
        q"""
          import cats.xml.utils.generic.TypeInfo
          import cats.xml.utils.generic.*
          import scala.reflect.runtime.universe.*

          TypeInfo.of[$wtpe](
            isString                         = $isString,
            isPrimitiveWrapper               = $isPrimitiveWrapper,
            isPrimitive                      = $isPrimitive,
            isValueClass                     = $isValueClass,
            isOptionOfAnyPrimitiveOrString   = $isOptionOfAnyPrimitiveOrString,
            accessorsInfo                    = $accessorsInfo,
            isNonPrimitiveValueClass         = $isNonPrimitiveValueClass
          )
         """
      )
    }

    def deriveFieldsTypeInfoImpl[T: c.WeakTypeTag](
      c: blackbox.Context
    ): c.Expr[Map[ParamName[T], TypeInfo[?]]] = {
      import c.universe.*

      val wtpe = weakTypeOf[T].finalResultType

      val tuples: List[Tree] = wtpe.members
        .collect {
          case mSymbol: MethodSymbol if mSymbol.isGetter && mSymbol.isPublic =>
            val name = mSymbol.name.toString
            q"""
           (ParamName[$wtpe]($name), TypeInfo.deriveTypeInfo[${mSymbol.returnType}])
         """
        }
        .toList
        .reverse

      c.Expr[Map[ParamName[T], TypeInfo[?]]](
        q"""
         import cats.xml.utils.generic.TypeInfo
         import cats.xml.utils.generic.*

         List(..$tuples).toMap
      """
      )
    }

    private class TypesUtils[C <: blackbox.Context](val c: C) {

      import c.universe.*

      // primitive
      def isString(tpe: c.universe.Type): Boolean =
        tpe <:< weakTypeOf[String]

      def isPrimitive(tpe: c.universe.Type): Boolean =
        tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isPrimitive

      def isPrimitiveWrapper(tpe: c.universe.Type): Boolean =
        List(
          weakTypeOf[BigDecimal],
          weakTypeOf[BigInt]
        ).exists(pWrapperTpe => tpe <:< pWrapperTpe)

      def isOptionOfAnyPrimitiveOrString(tpe: c.universe.Type): Boolean = {
        val typeArgs = tpe.typeArgs
        tpe <:< typeOf[Option[?]] &&
        typeArgs.nonEmpty &&
        (typeArgs.exists(isPrimitive) ||
          typeArgs.exists(isPrimitiveWrapper) ||
          typeArgs.exists(isString))
      }

      // value class
      def isValueClass(tpe: c.universe.Type): Boolean =
        tpe <:< typeOf[AnyVal] &&
          tpe.typeSymbol.isClass &&
          tpe.typeSymbol.asClass.isCaseClass &&
          getAccessors(tpe).size == 1

      def isNonPrimitiveValueClass(tpe: c.universe.Type): Boolean = {
        tpe <:< typeOf[AnyVal] &&
        tpe.typeSymbol.isClass &&
        tpe.typeSymbol.asClass.isCaseClass &&
        getAccessors(tpe).size == 1 &&
        getAccessors(getAccessors(tpe).head.returnType).nonEmpty
      }

      // utils
      def getAccessors(tpe: c.universe.Type): Iterable[c.universe.MethodSymbol] =
        tpe.members.collect {
          case m: MethodSymbol if m.isGetter && m.isPublic => m
        }
    }

  }
}
