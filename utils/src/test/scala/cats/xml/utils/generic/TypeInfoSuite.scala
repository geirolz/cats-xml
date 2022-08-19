package cats.xml.utils.generic

class TypeInfoSuite extends munit.FunSuite {

  case class Foo(a: String, b: Option[Int], c: Bar)
  case class Bar(d: String)

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
        accessorsInfo = Map(
          ParamName(
            value = "a"
          ) -> TypeInfo.of[String](
            isString                       = true,
            isPrimitiveWrapper             = false,
            isPrimitive                    = false,
            isValueClass                   = false,
            isOptionOfAnyPrimitiveOrString = false,
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
            accessorsInfo = Map(
              ParamName(
                value = "d"
              ) -> TypeInfo.of[String](
                isString                       = true,
                isPrimitiveWrapper             = false,
                isPrimitive                    = false,
                isValueClass                   = false,
                isOptionOfAnyPrimitiveOrString = false,
                accessorsInfo                  = Map.empty[ParamName[String], TypeInfo[?]]
              )
            )
          )
        )
      )
    )
  }
}
