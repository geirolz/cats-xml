package cats.xml.modifier

class ModifierSuite extends munit.FunSuite {

  test("Modifier.id") {
    assertEquals(
      obtained = Modifier.id[String]("FOO"),
      expected = ModifierResult.Modified("FOO")
    )
  }

  test("Modifier.const") {
    assertEquals(
      obtained = Modifier.const[String](ModifierResult.Modified("FOO"))("BAR"),
      expected = ModifierResult.Modified("FOO")
    )
  }

  test("Modifier.failed") {
    assertEquals(
      obtained = Modifier.failed[String](ModifierResult.Custom("BOOM!"))("BAR"),
      expected = ModifierResult.Custom("BOOM!")
    )
  }
}
