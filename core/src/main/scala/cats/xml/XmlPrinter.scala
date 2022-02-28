package cats.xml

import scala.collection.mutable

object XmlPrinter {

  def stringify(tree: XmlNode): String =
    prettyString(
      tree         = tree,
      returnOnNode = false,
      indentChar   = None,
      maxTextSize  = None
    )

  def prettyString(
    tree: XmlNode,
    returnOnNode: Boolean    = true,
    indentChar: Option[Char] = Some(' '),
    maxTextSize: Option[Int] = Some(30)
  ): String = {

    def build(
      tree: XmlNode,
      contentOpt: Option[String],
      deep: Int,
      isText: Boolean
    ): String = {

      val nodeName  = tree.label
      val nodeAttrs = tree.attributes.map(XmlAttribute.stringify).mkString(" ")
      val nodeInfo  = s"$nodeName${if (nodeAttrs.isEmpty) "" else " "}$nodeAttrs"
      val tabs = indentChar match {
        case Some(indentCharValue) => (0 until deep).map(_ => indentCharValue).mkString("")
        case None                  => ""
      }

      contentOpt match {
        case None =>
          s"$tabs<$nodeInfo/>"
        case Some(content) =>
          val exceedMaxText = maxTextSize match {
            case Some(maxValue) => content.length > maxValue
            case None           => false
          }
          val startContent =
            if (!isText && returnOnNode) s"\n" else if (exceedMaxText) s"\n$tabs " else ""
          val endContent = if ((!isText || exceedMaxText) && returnOnNode) s"\n$tabs" else ""
          s"$tabs<$nodeInfo>$startContent$content$endContent</$nodeName>"
      }
    }

    def rec(t: XmlNode, stringBuilder: mutable.StringBuilder, deep: Int): String =
      t.content match {
        case NodeContent.Empty =>
          stringBuilder
            .append(build(tree = t, contentOpt = None, deep = deep, isText = false))
            .toString()
        case NodeContent.Text(data) =>
          stringBuilder
            .append(
              build(tree = t, contentOpt = Some(data.toString), deep = deep, isText = true)
            )
            .toString()
        case NodeContent.Children(childrenNel) =>
          build(
            tree = t,
            contentOpt = Some(
              childrenNel
                .map(n => {
                  rec(n, new mutable.StringBuilder, deep + 1)
                })
                .toList
                .mkString("")
            ),
            deep   = deep,
            isText = false
          )
      }

    rec(
      t             = tree,
      stringBuilder = new mutable.StringBuilder(),
      deep          = 0
    )
  }
}
