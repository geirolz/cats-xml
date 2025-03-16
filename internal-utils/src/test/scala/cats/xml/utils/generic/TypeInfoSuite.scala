package cats.xml.utils.generic

object TypeInfoSuite {
  case class Foo(a: String, b: Option[Int], c: Bar, c2: Qux, c3: Quux)
  case class Bar(d: String)
  case class Baz(e: String)
  case class Qux(f: Baz) extends AnyVal
  case class Quux(g: String) extends AnyVal
}

import cats.xml.utils.generic.TypeInfoSuite.*

class TypeInfoSuite extends munit.FunSuite {

  test("TypeInfo.deriveTypeInfo derives the right information for the type Foo") {
    val fooTypeInfo: TypeInfo[Foo] = TypeInfo.deriveTypeInfo[Foo]
    assertEquals(
      obtained = fooTypeInfo,
      expected = TypeInfo.of[Foo](
        isString                       = false,
        isPrimitiveWrapper             = false,
        isPrimitive                    = false,
        isValueClass                   = false,
        isOptionOfAnyPrimitiveOrString = false,
        isNonPrimitiveValueClass       = false,
        accessorsInfo = Map(
          ParamName(
            value = "a"
          ) -> TypeInfo.of[String](
            isString                       = true,
            isPrimitiveWrapper             = false,
            isPrimitive                    = false,
            isValueClass                   = false,
            isOptionOfAnyPrimitiveOrString = false,
            isNonPrimitiveValueClass       = false,
            accessorsInfo                  = Map.empty[ParamName[String], TypeInfo[?]]
          ),
          ParamName(
            value = "b"
          ) -> TypeInfo.of[Option[Int]](
            isString                       = false,
            isPrimitiveWrapper             = false,
            isPrimitive                    = false,
            isValueClass                   = false,
            isOptionOfAnyPrimitiveOrString = true,
            isNonPrimitiveValueClass       = false,
            accessorsInfo                  = Map.empty[ParamName[Option[Int]], TypeInfo[?]]
          ),
          ParamName(
            value = "c"
          ) -> TypeInfo.of[Bar](
            isString                       = false,
            isPrimitiveWrapper             = false,
            isPrimitive                    = false,
            isValueClass                   = false,
            isOptionOfAnyPrimitiveOrString = false,
            isNonPrimitiveValueClass       = false,
            accessorsInfo = Map(
              ParamName(
                value = "d"
              ) -> TypeInfo.of[String](
                isString                       = true,
                isPrimitiveWrapper             = false,
                isPrimitive                    = false,
                isValueClass                   = false,
                isOptionOfAnyPrimitiveOrString = false,
                isNonPrimitiveValueClass       = false,
                accessorsInfo                  = Map.empty[ParamName[String], TypeInfo[?]]
              )
            )
          ),
          ParamName(
            value = "c2"
          ) -> TypeInfo.of[Qux](
            isString                       = false,
            isPrimitiveWrapper             = false,
            isPrimitive                    = false,
            isValueClass                   = true,
            isOptionOfAnyPrimitiveOrString = false,
            isNonPrimitiveValueClass       = true,
            accessorsInfo = Map(
              ParamName(
                value = "f"
              ) -> TypeInfo.of[Baz](
                isString                       = false,
                isPrimitiveWrapper             = false,
                isPrimitive                    = false,
                isValueClass                   = false,
                isOptionOfAnyPrimitiveOrString = false,
                isNonPrimitiveValueClass       = false,
                accessorsInfo = Map(
                  ParamName(
                    value = "e"
                  ) -> TypeInfo.of[String](
                    isString                       = true,
                    isPrimitiveWrapper             = false,
                    isPrimitive                    = false,
                    isValueClass                   = false,
                    isOptionOfAnyPrimitiveOrString = false,
                    isNonPrimitiveValueClass       = false,
                    accessorsInfo                  = Map.empty[ParamName[String], TypeInfo[?]]
                  )
                )
              )
            )
          ),
          ParamName(
            value = "c3"
          ) -> TypeInfo.of[Quux](
            isString                       = false,
            isPrimitiveWrapper             = false,
            isPrimitive                    = false,
            isValueClass                   = true,
            isOptionOfAnyPrimitiveOrString = false,
            isNonPrimitiveValueClass       = false,
            accessorsInfo = Map(
              ParamName(
                value = "g"
              ) -> TypeInfo.of[String](
                isString                       = true,
                isPrimitiveWrapper             = false,
                isPrimitive                    = false,
                isValueClass                   = false,
                isOptionOfAnyPrimitiveOrString = false,
                isNonPrimitiveValueClass       = false,
                accessorsInfo                  = Map.empty[ParamName[String], TypeInfo[?]]
              )
            )
          )
        )
      )
    )
  }
}
