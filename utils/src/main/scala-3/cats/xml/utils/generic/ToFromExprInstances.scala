package cats.xml.utils.generic

private[generic] object ToFromExprInstances {

  import scala.quoted.*

  given [T: Type]: ToExpr[ParamName[T]] = new ToExpr[ParamName[T]] {
    def apply(x: ParamName[T])(using Quotes): Expr[ParamName[T]] =
      '{ ParamName[T](${ Expr(x.value) }) }
  }
}
