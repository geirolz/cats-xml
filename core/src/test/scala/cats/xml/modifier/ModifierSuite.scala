package cats.xml.modifier

class ModifierSuite extends munit.FunSuite {

  test("Modifier.id") {
    assertEquals(
      obtained = Modifier.id[String]("FOO"),
      expected = Right("FOO")
    )
  }

  test("Modifier.const") {
    assertEquals(
      obtained = Modifier.const[String](Right("FOO"))("BAR"),
      expected = Right("FOO")
    )
  }

  test("Modifier.failed") {
    assertEquals(
      obtained = Modifier.failed[String](ModifierFailure.Custom("BOOM!"))("BAR"),
      expected = Left(ModifierFailure.Custom("BOOM!"))
    )
  }
}
