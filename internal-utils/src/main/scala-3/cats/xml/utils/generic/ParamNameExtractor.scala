package cats.xml.utils.generic

import scala.annotation.{tailrec, unused}
import scala.reflect.macros.blackbox
import cats.xml.utils.generic.ParamName
import scala.quoted.*

class ParamNameExtractor[T] private () {
  inline def param[U](path: T => U): ParamName[T] =
    ${ ParamNameExtractor.Scala3Macros.extractParamName[T, U]('path) }
}
object ParamNameExtractor {

  def of[T]: ParamNameExtractor[T] = new ParamNameExtractor[T]

  object Scala3Macros {
    def extractParamName[T: Type, U: Type](path: Expr[T => U])(using Quotes): Expr[ParamName[T]] =
      new ParamNameExtractor.Scala3Macros().extractParamNameImpl[T, U](path)
  }
  class Scala3Macros(using q: Quotes) {

    import q.reflect.*
    import ToFromExprInstances.given

    def extractParamNameImpl[T: Type, U: Type](path: Expr[T => U]): Expr[ParamName[T]] = {

      import quotes.reflect.*

      val expectedShapeInfo = "Path must have shape: _.field1.field2.each.field3.(...)"

      enum PathElement {
        case TermPathElement(term: String, xargs: String*) extends PathElement
        case FunctorPathElement(functor: String, method: String, xargs: String*) extends PathElement
      }

      @tailrec
      def toPath(tree: Tree, acc: List[PathElement]): Seq[PathElement] = {
        tree match {

          /** Field access */
          case Select(deep, ident) =>
            toPath(deep, PathElement.TermPathElement(ident) :: acc)

          /** The first segment from path (e.g. `_.age` -> `_`) */
          case i: Ident =>
            acc
          case t =>
            report.errorAndAbort(s"Unsupported path element $t")
        }
      }

      val pathElements: Seq[PathElement] = path.asTerm match {

        /** Single inlined path */
        case Inlined(_, _, Block(List(DefDef(_, _, _, Some(p))), _)) =>
          toPath(p, List.empty)
//        case Expr(_, _, Block(List(DefDef(_, _, _, Some(p))), _)) =>
//          toPath(p, List.empty)
        case _ =>
          report.errorAndAbort(s"Unsupported path [${path.show}]")
      }

      Expr(
        ParamName[T](
          pathElements
            .map {
              case PathElement.TermPathElement(c, _ @_*)            => c
              case PathElement.FunctorPathElement(_, method, _ @_*) => method
            }
            .mkString(".")
        )
      )
    }
  }
}
