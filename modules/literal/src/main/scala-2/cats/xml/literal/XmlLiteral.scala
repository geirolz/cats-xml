package cats.xml.literal

import cats.xml.XmlNode

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.Method
import java.util.UUID
import scala.annotation.unused
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

class XmlLiteralMacros(val c: blackbox.Context) {

  import c.universe.*

  private[this] def parserClass: Class[?] = Class.forName("org.typelevel.jawn.Parser$")
  private[this] def parseMethod: Method   = parserClass.getMethod("parseString", classOf[String])

  /** Represents an interpolated expression that we've replaced with a unique string during parsing.
    */
  private[this] class Replacement(val placeholder: String, argument: Tree) {
    private[this] val argumentType = c.typecheck(argument).tpe
    def asXml: c.Expr[XmlNode] =
      c.Expr(q"_root_.cats.xml.codec.Encoder[${argumentType.widen}].encode($argument)")
  }

  private[this] object Replacement {
    private[this] final def generatePlaceholder(): String = UUID.randomUUID().toString

    def apply(stringParts: Seq[String], argument: Tree): Replacement = {

      /** Generate a unique string that doesn't appear in the XML literal.
        */
      val placeholder =
        LazyList
          .continually(generatePlaceholder())
          .distinct
          .dropWhile(s => stringParts.exists(_.contains(s)))
          .head

      new Replacement(placeholder, argument)
    }
  }

  final def parseXmlLiteralString(args: c.Expr[Any]*): Tree = {

    c.prefix.tree match {
      case Apply(_, Apply(_, parts) :: Nil) =>
        val stringParts = parts.map {
          case Literal(Constant(part: String)) => part
          case _ =>
            c.abort(
              c.enclosingPosition,
              "A StringContext part for the xml interpolator is not a string"
            )
        }

        val replacements: Seq[Replacement] =
          args.map(argument => Replacement(stringParts, argument.tree))

        if (stringParts.size != replacements.size + 1)
          c.abort(
            c.enclosingPosition,
            "Invalid arguments for the xml interpolator"
          )
        else {
          val xmlString = stringParts.zip(replacements.map(_.placeholder)).foldLeft("") {
            case (acc, (part, placeholder)) =>
              val qm = "\""

              s"$acc$part$qm$placeholder$qm"
          } + stringParts.last

          parse(xmlString, replacements) match {
            case Right(tree) => tree
            case Left(t: Throwable) =>
              val sw = new StringWriter
              t.printStackTrace(new PrintWriter(sw))
              c.abort(c.enclosingPosition, s"Invalid XML in interpolated string, ${sw.toString}")
          }
        }
      case _ => c.abort(c.enclosingPosition, "Invalid use of the xml interpolator")
    }
  }

  private[this] final def parse(
    xmlString: String,
    @unused replacements: Seq[Replacement]
  ): Either[Throwable, Tree] = {

    try {
      val result: XmlNode =
        c.eval(c.Expr(q"_root_.cats.xml.XmlParser.xmlParserForTry.parseString($xmlString).get"))
      Right(q"$result")
    } catch {
      case NonFatal(e) => Left(e)
    }
  }

}
trait XmlLiteralSyntax {

  implicit final class XmlLiteralStringCtxOps(sc: StringContext) {
    def xml(args: Any*): XmlNode = macro XmlLiteralMacros.parseXmlLiteralString
  }
}
