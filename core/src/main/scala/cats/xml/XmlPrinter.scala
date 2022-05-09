package cats.xml

import cats.xml.Xml.XmlNull

import scala.collection.mutable

object XmlPrinter {

  case class Config(
    returnOnNode: Boolean    = true,
    indentChar: Option[Char] = Some(' '),
    maxTextSize: Option[Int] = Some(30)
  )
  object Config {
    implicit val default: Config = Config()
  }

  def stringify(xml: Xml): String =
    prettyString(xml = xml)(
      Config(
        returnOnNode = false,
        indentChar   = None,
        maxTextSize  = None
      )
    )

  def prettyString(xml: Xml)(implicit config: Config): String = {

    def buildNode(
      tree: XmlNode,
      contentOpt: Option[String],
      deep: Int,
      isText: Boolean
    ): String = {

      val nodeName  = tree.label
      val nodeAttrs = tree.attributes.map(prettyString).mkString(" ")
      val nodeInfo  = s"$nodeName${if (nodeAttrs.isEmpty) "" else " "}$nodeAttrs"
      val tabs = config.indentChar match {
        case Some(indentCharValue) => (0 until deep).map(_ => indentCharValue).mkString("")
        case None                  => ""
      }

      contentOpt match {
        case None =>
          s"$tabs<$nodeInfo/>"
        case Some(content) =>
          val exceedMaxText = config.maxTextSize match {
            case Some(maxValue) => content.length > maxValue
            case None           => false
          }
          val startContent =
            if (!isText && config.returnOnNode) s"\n" else if (exceedMaxText) s"\n$tabs " else ""
          val endContent = if ((!isText || exceedMaxText) && config.returnOnNode) s"\n$tabs" else ""
          s"$tabs<$nodeInfo>$startContent$content$endContent</$nodeName>"
      }
    }

    def recNode(t: XmlNode, stringBuilder: mutable.StringBuilder, deep: Int): String =
      t.content match {
        case NodeContent.Empty =>
          stringBuilder
            .append(buildNode(tree = t, contentOpt = None, deep = deep, isText = false))
            .toString()
        case NodeContent.Text(data) =>
          stringBuilder
            .append(
              buildNode(tree = t, contentOpt = Some(prettyString(data)), deep = deep, isText = true)
            )
            .toString()
        case NodeContent.Children(childrenNel) =>
          buildNode(
            tree = t,
            contentOpt = Some(
              childrenNel
                .map(n => {
                  recNode(n, new mutable.StringBuilder, deep + 1)
                })
                .toList
                .mkString("")
            ),
            deep   = deep,
            isText = false
          )
      }

    xml match {
      case XmlNull            => ""
      case attr: XmlAttribute => attr.asString
      case data: XmlData      => data.asString
      case node: XmlNode =>
        recNode(
          t             = node,
          stringBuilder = new mutable.StringBuilder(),
          deep          = 0
        )
    }

  }
}
