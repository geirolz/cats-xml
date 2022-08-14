package cats.xml.utils.generic

class TypeInfoSuite extends munit.FunSuite {

  case class Foo(a: String, b: Option[Int], c: Bar)
  case class Bar(d: String)

  test("TypeInfo.deriveTypeInfo derives the right information for the type Foo") {
    val fooTypeInfo: TypeInfo[Foo] = TypeInfo.deriveTypeInfo[Foo]
    assertEquals(
      obtained = fooTypeInfo,
      expected = TypeInfo.of(
        isString           = false,
        isPrimitiveWrapper = false,
        isPrimitive        = false,
        isValueClass       = false,
        accessorsInfo = Map(
          ParamName(
            value = "a"
          ) -> TypeInfo.of(
            isString           = true,
            isPrimitiveWrapper = false,
            isPrimitive        = false,
            isValueClass       = false,
            accessorsInfo      = Map()
          ),
          ParamName(
            value = "b"
          ) -> TypeInfo.of(
            isString           = false,
            isPrimitiveWrapper = false,
            isPrimitive        = false,
            isValueClass       = false,
            accessorsInfo      = Map()
          ),
          ParamName(
            value = "c"
          ) -> TypeInfo.of(
            isString           = false,
            isPrimitiveWrapper = false,
            isPrimitive        = false,
            isValueClass       = false,
            accessorsInfo = Map(
              ParamName(
                value = "d"
              ) -> TypeInfo.of(
                isString           = true,
                isPrimitiveWrapper = false,
                isPrimitive        = false,
                isValueClass       = false,
                accessorsInfo      = Map()
              )
            )
          )
        )
      )
    )
  }
}
