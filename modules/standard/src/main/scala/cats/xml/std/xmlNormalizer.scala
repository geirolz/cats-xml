package cats.xml.std

import scala.xml.{Elem, Node, NodeSeq, Text}

object XmlNormalizer extends XmlNormalizerSyntax {
  def normalize(ns: NodeSeq): NodeSeq = {

    def trimTextZappingEmpty(node: Node): Seq[Node] =
      node match {
        case Text(text) if text.trim.isEmpty => Nil
        case Text(text)                      => List(Text(text.trim))
        case Elem(pre, lab, md, scp, children*) =>
          Elem(pre, lab, md, scp, false, children.flatMap(trimTextZappingEmpty)*)
        case _ => List(node)
      }

    def mergeTextNode(children: Seq[Node]): Seq[Node] =
      children.foldRight(List.empty[Node]) { (ele, acc) =>
        ele match {
          case eleTxt: Text =>
            acc.headOption match {
              case Some(accTxt: Text) => Text(accTxt.text + eleTxt.text) :: acc.tail
              case _                  => ele :: acc
            }
          case _ => ele :: acc
        }
      }

    ns flatMap {
      case el: Elem =>
        List(
          el.copy(
            child         = normalize(mergeTextNode(el.child).flatMap(trimTextZappingEmpty)),
            minimizeEmpty = true
          )
        )
      case text: Text => trimTextZappingEmpty(text)
      case x          => List(x)
    }
  }
}
private[std] trait XmlNormalizerSyntax {

  implicit class NodeSeqNormalizationAndEqualityOps[N <: NodeSeq](ns: N) {
    def normalized: NodeSeq =
      XmlNormalizer.normalize(ns)
  }
}
