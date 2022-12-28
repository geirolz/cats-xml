package cats.xml

import cats.xml.Xml.XmlNull
import cats.xml.utils.format.Indentator
import cats.xml.utils.Debug
import cats.Show
import cats.xml.XmlPrinter.Config

import scala.annotation.tailrec
import scala.collection.mutable

trait XmlPrinter {

  def stringify(xml: Xml): String =
    prettyString(xml = xml)(
      Config(
        nodeReturnChar = None,
        indentSize     = 0,
        maxTextSize    = None
      )
    )

  def prettyString(xml: Xml)(implicit config: Config): String
}

object XmlPrinter {

  val space: Char   = ' '
  val newLine: Char = '\n'

  case class Config(
    nodeReturnChar: Option[Char] = Some(newLine),
    indentChar: Char             = space,
    indentSize: Int              = 1,
    maxTextSize: Option[Int]     = Some(30)
  )
  object Config {
    implicit val default: Config = Config()
  }

  // --------------------------------------------------------------//
  implicit val default: XmlPrinter = new XmlPrinter {
    override def prettyString(xml: Xml)(implicit config: Config): String = {
      val returnString: String =
        config.nodeReturnChar
          .map(_.toString)
          .getOrElse("")

      def exceedTextMaxSize(textSize: Int): Boolean =
        config.maxTextSize.exists(_ < textSize)

      @tailrec
      def recAppendXml(
        ls: List[Xml],
        acc: mutable.StringBuilder,
        indentator: Indentator,
        prefixSeparator: String  = "",
        postfixSeparator: String = "",
        latestSeparator: String  = "",
        transform: String => String
      ): mutable.StringBuilder = {

        def append(str: String): mutable.StringBuilder =
          acc.append(transform(str))

        ls match {
          case Nil =>
            append(latestSeparator)
          case ::(head, tail) =>
            append(prefixSeparator)
            recAppend(
              xml        = head,
              acc        = acc,
              indentator = indentator,
              transform  = transform
            )
            if (tail.nonEmpty) append(postfixSeparator)
            recAppendXml(
              ls               = tail,
              acc              = acc,
              indentator       = indentator,
              prefixSeparator  = prefixSeparator,
              postfixSeparator = postfixSeparator,
              latestSeparator  = latestSeparator,
              transform        = transform
            )
        }
      }

      def recAppend(
        xml: Xml,
        acc: mutable.StringBuilder,
        indentator: Indentator,
        transform: String => String = identity
      ): mutable.StringBuilder = {

        def append(str: String): mutable.StringBuilder =
          acc.append(transform(str))

        xml match {
          case XmlNull => acc
          case attr: XmlAttribute =>
            if (Debug.enabledAnd(_.xmlPrinterPrintTypesName)) {
              attr.value match {
                case Xml.Null => append(s"${attr.key}=\"NULL\"")
                case _ =>
                  append(
                    s"${attr.key}=\"${Show[XmlData].show(attr.value)}:${attr.value.getClass.getTypeName}\""
                  )
              }
            } else {
              attr.value match {
                case Xml.Null => acc
                case _        => append(s"${attr.key}=\"${Show[XmlData].show(attr.value)}\"")
              }
            }
          case data: XmlData =>
            append(
              Debug.ifEnabledAnd(_.xmlPrinterPrintTypesName)(
                ifTrue  = s"${data.asString}:${data.getClass.getTypeName}",
                ifFalse = data.asString
              )
            )
          case group: XmlNode.Group =>
            recAppendXml(
              ls               = group.children.toList,
              acc              = acc,
              indentator       = indentator,
              postfixSeparator = returnString,
              transform        = transform
            )
          case node: XmlNode =>
            val nodeName    = node.label
            val nodeContent = node.content

            // indent
            append(indentator.indentation)

            // open head-node
            append(s"<$nodeName")

            // append attrs
            recAppendXml(
              ls              = node.attributes,
              acc             = acc,
              indentator      = indentator,
              prefixSeparator = " ",
              transform       = transform
            )

            // close head-node
            if (nodeContent.isEmpty)
              append(s"/>")
            else
              append(s">")

            // append content
            nodeContent match {
              case NodeContent.Empty => ()
              case NodeContent.Text(data) =>
                recAppend(
                  xml        = data,
                  acc        = acc,
                  indentator = indentator.forward,
                  transform = transform.andThen(str => {
                    if (nodeContent.isText && exceedTextMaxSize(str.length))
                      s"$returnString${indentator.forward.indentation}$str$returnString"
                    else
                      str
                  })
                )
              case NodeContent.Children(childrenNel) =>
                append(returnString)
                recAppendXml(
                  ls               = childrenNel.toList,
                  acc              = acc,
                  indentator       = indentator.forward,
                  postfixSeparator = returnString,
                  transform        = transform
                )
            }

            // return after node content if necessary
            if (!nodeContent.isEmpty && !nodeContent.isText) {

              // indent
              append(returnString)

              // indent
              append(indentator.indentation)
            }

            // close tail node
            if (!nodeContent.isEmpty)
              append(s"</$nodeName>")

            acc
        }
      }

      recAppend(
        xml = xml,
        acc = new mutable.StringBuilder(),
        indentator = Indentator.root(
          char = config.indentChar,
          size = config.indentSize
        )
      ).toString()
    }
  }
}
