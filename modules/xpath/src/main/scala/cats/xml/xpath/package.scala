package cats.xml

import eu.cdevreeze.xpathparser.ast.EQName

package object xpath {
  private[xpath] object EmptySeq {
    def unapply(io: Iterable[?]): Boolean = io.isEmpty
  }

  private[xpath] object UnSeq {
    def unapplySeq(io: Iterable[?]): Option[Seq[?]] = Some(io.toSeq)
  }

  private[xpath] object EQNameEx {
    def unapply(eqn: EQName): Some[String] =
      eqn match {
        case EQName.QName(qname)            => Some(qname.localPart)
        case EQName.URIQualifiedName(ename) => Some(ename.localPart)
      }
  }
}
