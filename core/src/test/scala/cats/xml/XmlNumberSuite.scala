package cats.xml

//TODO
class XmlNumberSuite extends munit.ScalaCheckSuite {

  test("") {

    Console.println(Xml.fromDataString("5340595900475325933418219074917").getClass)
    assertEquals(
      obtained = Xml.fromDataString("5340595900475325933418219074917"),
      expected = Xml.ofString("5340595900475325933418219074917")
    )
  }
}
