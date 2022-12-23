package cats.xml.utils

class UnderlyingThrowableWeakEqSuite extends munit.FunSuite {

  case class UTEW1(error: Throwable) extends UnderlyingThrowableWeakEq
  case class UTEW2(error: Throwable) extends UnderlyingThrowableWeakEq

  // -------------- eqv --------------
  test("UnderlyingThrowableWeakEq.Eq is eqv if exception is the same") {
    val ex = new RuntimeException("ERROR")

    assert(
      cats
        .Eq[UnderlyingThrowableWeakEq]
        .eqv(
          UTEW1(ex),
          UTEW2(ex)
        )
    )
  }

  test(
    "UnderlyingThrowableWeakEq.Eq is eqv if the class type, the cause the and message are equals"
  ) {
    val cause = new RuntimeException("CAUSE")
    val ex1   = new RuntimeException("ERROR", cause)
    val ex2   = new RuntimeException("ERROR", cause)

    assert(
      cats
        .Eq[UnderlyingThrowableWeakEq]
        .eqv(
          UTEW1(ex1),
          UTEW2(ex2)
        )
    )
  }

  // -------------- neqv --------------
  test("UnderlyingThrowableWeakEq.Eq is neqv if the type is neqv") {
    val ex1 = new RuntimeException("ERROR")
    val ex2 = new Exception("ERROR")

    assert(
      cats
        .Eq[UnderlyingThrowableWeakEq]
        .neqv(
          UTEW1(ex1),
          UTEW2(ex2)
        )
    )
  }

  test("UnderlyingThrowableWeakEq.Eq is neqv if the cause is neqv") {
    val ex1 = new RuntimeException("ERROR", new RuntimeException("C1"))
    val ex2 = new RuntimeException("ERROR", new RuntimeException("C2"))

    assert(
      cats
        .Eq[UnderlyingThrowableWeakEq]
        .neqv(
          UTEW1(ex1),
          UTEW2(ex2)
        )
    )
  }

  test("UnderlyingThrowableWeakEq.Eq is neqv if the message is neqv") {
    val ex1 = new RuntimeException("ERROR 1")
    val ex2 = new RuntimeException("ERROR 2")

    assert(
      cats
        .Eq[UnderlyingThrowableWeakEq]
        .neqv(
          UTEW1(ex1),
          UTEW2(ex2)
        )
    )
  }
}
