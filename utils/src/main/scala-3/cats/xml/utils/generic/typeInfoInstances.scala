package cats.xml.utils.generic

import scala.quoted.*

trait TypeInfoInstances {

  inline given deriveTypeInfo[T]: TypeInfo[T] =
    ${ TypeInfoInstances.Scala3Macros.deriveTypeInfo[T] }

  inline given deriveFieldsTypeInfo[T]: Map[ParamName[T], TypeInfo[?]] =
    ${ TypeInfoInstances.Scala3Macros.deriveFieldsTypeInfo[T] }
}
object TypeInfoInstances {

  object Scala3Macros {

    def deriveTypeInfo[T: Type](using Quotes): Expr[TypeInfo[T]] =
      new TypeInfoInstances.Scala3Macros().deriveTypeInfoImpl[T]

    def deriveFieldsTypeInfo[T: Type](using Quotes): Expr[Map[ParamName[T], TypeInfo[?]]] =
      new TypeInfoInstances.Scala3Macros().deriveFieldsTypeInfoImpl[T]
  }
  class Scala3Macros(using q: Quotes) {

    import q.reflect.*
    private val utils: TypesUtils                     = new TypesUtils
    private var cache: Map[String, Expr[TypeInfo[?]]] = Map.empty

    def deriveTypeInfoImpl[T: Type]: Expr[TypeInfo[T]] = {

      def deriveNewType[T: Type]: Expr[TypeInfo[T]] = {
        val accessorsInfo: Expr[Map[ParamName[T], TypeInfo[?]]] = deriveFieldsTypeInfoImpl[T]
        val isString: Expr[Boolean]                             = Expr(utils.isString[T])
        val isPrimitiveWrapper: Expr[Boolean]                   = Expr(utils.isPrimitiveWrapper[T])
        val isPrimitive: Expr[Boolean]                          = Expr(utils.isPrimitive[T])
        val isValueClass: Expr[Boolean]                         = Expr(utils.isValueClass[T])
        val isOptionOfAnyPrimitiveOrString: Expr[Boolean] = Expr(
          utils.isOptionOfAnyPrimitiveOrString[T]
        )

        '{
          TypeInfo.of[T](
            isString                       = ${ isString },
            isPrimitiveWrapper             = ${ isPrimitiveWrapper },
            isPrimitive                    = ${ isPrimitive },
            isValueClass                   = ${ isValueClass },
            isOptionOfAnyPrimitiveOrString = ${ isOptionOfAnyPrimitiveOrString },
            accessorsInfo                  = ${ accessorsInfo }
          )
        }
      }

      val key = TypeRepr.of[T].show

      cache.get(key) match {
        case Some(value) =>
          value.asExprOf[TypeInfo[T]]
        case None =>
          val tpeInfoExpr: Expr[TypeInfo[T]] = deriveNewType[T]
          cache = cache.updated(key, tpeInfoExpr)
          tpeInfoExpr
      }
    }

    def deriveFieldsTypeInfoImpl[T: Type]: Expr[Map[ParamName[T], TypeInfo[?]]] = {

      val tpe: TypeRepr = TypeRepr.of[T]

      val tuples: List[Expr[(ParamName[T], TypeInfo[?])]] =
        tpe.typeSymbol.caseFields
          .map(f => {

            val fieldTypeRepr: TypeRepr = tpe.memberType(f)
            val name: Expr[String]      = Expr(f.name)

            fieldTypeRepr.asType match {
              case '[t] =>
                '{
                  (
                    ParamName[T](${ name }),
                    ${ deriveTypeInfoImpl[t] }
                  )
                }
              case _ => report.errorAndAbort("Unsupported operation exception")
            }
          })
          .toList

      '{ ${ Varargs(tuples) }.toMap }
    }

    private class TypesUtils(using val q: Quotes) {

      import q.reflect.*

      private val anyValTypeRepr: TypeRepr = TypeRepr.of[AnyVal]

      private val primitivesWrapperTypeSymbols: List[Symbol] =
        List(
          TypeRepr.of[BigDecimal],
          TypeRepr.of[BigInt]
        ).map(_.typeSymbol)

      private val allPrimitivesAndStringTypeSymbols: List[Symbol] =
        (primitivesWrapperTypeSymbols ++ defn.ScalaPrimitiveValueClasses) :+ defn.StringClass

      // primitive
      def isString[T: Type]: Boolean =
        TypeRepr.of[T].typeSymbol == defn.StringClass

      def isPrimitive[T: Type]: Boolean =
        defn.ScalaPrimitiveValueClasses
          .contains(TypeRepr.of[T].typeSymbol)

      def isPrimitiveWrapper[T: Type]: Boolean =
        primitivesWrapperTypeSymbols
          .contains(TypeRepr.of[T].typeSymbol)

      // option
      def isOptionOfAnyPrimitiveOrString[T: Type]: Boolean =
        TypeRepr.of[T] match {
          case at: AppliedType =>
            val (tycon, args) = AppliedType.unapply(at)
            tycon.typeSymbol == defn.OptionClass
            && args.headOption.exists(argT =>
              allPrimitivesAndStringTypeSymbols.contains(argT.typeSymbol)
            )
          case _ => false
        }

      // value class
      def isValueClass[T: Type]: Boolean = {
        val tpe = TypeRepr.of[T]
        (tpe <:< anyValTypeRepr)
        && tpe.classSymbol.exists(cs => cs.flags.is(Flags.Case) && cs.isClassDef)
        && getAccessors[T].size == 1
      }

      // utils
      def getAccessors[T: Type]: List[Symbol] =
        TypeRepr.of[T].typeSymbol.fieldMembers
    }
  }

}
