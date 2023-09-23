package cats.xml

import cats.xml.utils.format.Indentator
import cats.xml.utils.Debug
import cats.xml.XmlData.*
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

      def showXmlData[T <: XmlData](data: T): String = {
        val strValue = data match {
          case XmlNull =>
            Debug.ifEnabledAnd(_.xmlPrinterPrintTypesName)(
              ifTrue  = "NULL",
              ifFalse = ""
            )
          case XmlString(value)     => value
          case XmlChar(value)       => value.toString
          case XmlBool(value)       => value.toString
          case XmlArray(value)      => value.mkString(",")
          case XmlLong(value)       => value.toString
          case XmlFloat(value)      => value.toString
          case XmlDouble(value)     => value.toString
          case XmlBigDecimal(value) => value.toString
        }

        Debug.ifEnabledAnd(_.xmlPrinterPrintTypesName)(
          ifTrue  = s"$strValue:${data.getClass.getTypeName}",
          ifFalse = strValue
        )
      }

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
          case Xml.Null => acc
          case attr: XmlAttribute =>
            attr.value match {
              case Xml.Null => acc
              case _        => append(s"${attr.key}=\"${showXmlData(attr.value)}\"")
            }
          case data: XmlData =>
            append(showXmlData(data))
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
