package cats.xml.utils

class BooleanUtilsSuite extends munit.FunSuite {

  import BooleanUtils.*

  test("TernaryOp is eq to If-construct") {

    val isZero: Int => Boolean = _ == 0
    val trueBranch: String     = "zero"
    val falseBranch: String    = "not zero"

    def execIfWith(a: Int): String =
      if (isZero(a)) trueBranch else falseBranch

    def execTernaryWith(a: Int): String =
      isZero(a) ? trueBranch | falseBranch

    Seq(0, 1).foreach(a =>
      assertEquals(
        execTernaryWith(a),
        execIfWith(a)
      )
    )
  }

  test("TernaryOp is eq to pattern matching") {

    val isZero: Int => Boolean = _ == 0
    val trueBranch: String     = "zero"
    val falseBranch: String    = "not zero"

    def execPatternMatching(a: Int): String =
      a match {
        case x if isZero(x) => trueBranch
        case _              => falseBranch
      }

    def execTernaryWith(a: Int): String =
      isZero(a) ? trueBranch | falseBranch

    Seq(0, 1).foreach(a =>
      assertEquals(
        execTernaryWith(a),
        execPatternMatching(a)
      )
    )
  }
}
