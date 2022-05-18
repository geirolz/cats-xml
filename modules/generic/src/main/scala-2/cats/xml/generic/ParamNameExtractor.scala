package cats.xml.generic

import cats.xml.generic.ParamNameExtractor.Macros

import scala.annotation.{tailrec, unused}
import scala.reflect.macros.blackbox

class ParamNameExtractor[T] private () {
  def param[U](@unused path: T => U): ParamName[T] =
    macro Macros.extract[T, U]
}
object ParamNameExtractor {

  private[generic] def of[T]: ParamNameExtractor[T] = new ParamNameExtractor[T]

  object Macros {

    def extract[T: c.WeakTypeTag, U: c.WeakTypeTag](
      c: blackbox.Context
    )(
      path: c.Expr[T => U]
    ): c.Expr[ParamName[T]] = {
      import c.universe.*

      val expectedShapeInfo = "Path must have shape: _.field1"
      case class TermPathElement(term: c.TermName, xargs: c.Tree*)

      @tailrec
      def collectPathElements(tree: c.Tree, acc: List[TermPathElement]): List[TermPathElement] = {

        tree match {
          case q"$parent.$child " => collectPathElements(parent, TermPathElement(child) :: acc)
          case _: Ident           => acc
          case _ =>
            c.abort(
              c.enclosingPosition,
              s"Unsupported path element. $expectedShapeInfo, got: $tree"
            )
        }
      }

      path.tree match {
        case q"($_) => $pathBody" =>
          collectPathElements(pathBody, Nil).headOption
            .collect { case TermPathElement(el) =>
              c.Expr[ParamName[T]](
                q"cats.xml.generic.ParamName[${weakTypeTag[T]}](${el.decodedName.toString})"
              )
            }
            .getOrElse(c.abort(c.enclosingPosition, s"$expectedShapeInfo, got: ${path.tree}"))
        case _ => c.abort(c.enclosingPosition, s"$expectedShapeInfo, got: ${path.tree}")
      }
    }
  }

}
