package cats.xml.modifier

class ModifierResultSuite extends munit.FunSuite {

  test("Modifier.Modified - isModified") {
    assert(ModifierResult.Modified("FOO").isModified)
  }
}
