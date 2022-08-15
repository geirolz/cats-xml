package cats.xml.utils

import munit.Location

class StringOpsSuite extends munit.FunSuite {

  testStringMapper(StringMapper.CamelCase)(
    "test"     -> "test",
    "testFoo"  -> "testFoo",
    "test-foo" -> "testFoo",
    "test_foo" -> "testFoo"
  )

  testStringMapper(StringMapper.KebabCase)(
    "test"     -> "test",
    "testFoo"  -> "test-foo",
    "test-foo" -> "test-foo",
    "test_foo" -> "test-foo"
  )

  testStringMapper(StringMapper.SnakeCase)(
    "test"     -> "test",
    "testFoo"  -> "test_foo",
    "test-foo" -> "test_foo",
    "test_foo" -> "test_foo"
  )

  testStringMapper(StringMapper.PascalCase)(
    "test"     -> "Test",
    "testFoo"  -> "TestFoo",
    "test-foo" -> "TestFoo",
    "test_foo" -> "TestFoo"
  )

  private def testStringMapper(
    mapper: StringMapper
  )(values: (String, String)*)(implicit loc: Location): Unit =
    test(s"StringMapper.${mapper.getClass.getSimpleName.replace("$", "")}") {
      values.foreach { case (input, expected) =>
        assertEquals(
          obtained = mapper(input),
          expected = expected
        )
      }
    }
}
