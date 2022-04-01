package cats.xml.testing

import cats.data.NonEmptyList
import cats.xml.{NodeContent, XmlAttribute, XmlNode}
import org.scalacheck.Gen

object XmlNodeGen {

  def genXmlNode(
    size: DataSize,
    maxNodeName: Int      = 10,
    maxAttrNameSize: Int  = 10,
    maxAttrValueSize: Int = 10,
    maxTextSize: Int      = 100
  ): Gen[XmlNode] = {

    def compile(maxAttrs: Int, maxChildren: Int, maxDeep: Int): Gen[XmlNode] =
      _genXmlNode(
        maxAttrs         = maxAttrs,
        maxChildren      = maxChildren,
        maxDeep          = maxDeep,
        maxNodeName      = maxNodeName,
        maxAttrNameSize  = maxAttrNameSize,
        maxAttrValueSize = maxAttrValueSize,
        maxTextSize      = maxTextSize
      )

    size match {
      case DataSize.S =>
        compile(
          maxAttrs    = 2,
          maxChildren = 2,
          maxDeep     = 2
        )
      case DataSize.M =>
        compile(
          maxAttrs    = 5,
          maxChildren = 4,
          maxDeep     = 4
        )
      case DataSize.L =>
        compile(
          maxAttrs    = 10,
          maxChildren = 6,
          maxDeep     = 6
        )
      case DataSize.XL =>
        compile(
          maxAttrs    = 20,
          maxChildren = 8,
          maxDeep     = 8
        )
    }
  }

  private def _genXmlNode(
    maxAttrs: Int,
    maxChildren: Int,
    maxDeep: Int,
    maxNodeName: Int      = 10,
    maxAttrNameSize: Int  = 10,
    maxAttrValueSize: Int = 10,
    maxTextSize: Int      = 100
  ): Gen[XmlNode] = {

    def genChildren: Gen[Option[NodeContent.Children]] =
      if (maxDeep > 0)
        Gen.lzy(
          Gen
            .choose(0, maxChildren)
            .flatMap(n =>
              Gen.listOfN(
                n = n,
                g = Gen.lzy(
                  _genXmlNode(
                    maxAttrs    = maxAttrs,
                    maxChildren = maxChildren,
                    maxDeep     = maxDeep - 1
                  )
                )
              )
            )
            .map(NonEmptyList.fromList(_).map(NodeContent.Children(_)))
        )
      else
        Gen.const(None)

    for {
      nodeName   <- Gen.lzy(getNonEmptyString(maxNodeName))
      attributes <- Gen.lzy(genXmlAttributes(maxAttrs, maxAttrNameSize, maxAttrValueSize))
      content <- Gen.lzy(
        Gen.frequency(
          2  -> Gen.const(NodeContent.empty),
          18 -> Gen.lzy(getNonEmptyString(maxTextSize).map(NodeContent.text(_))),
          80 -> Gen.lzy(genChildren.map(_.getOrElse(NodeContent.empty)))
        )
      )
    } yield XmlNode(nodeName)
      .withAttributes(attributes)
      .withContent(content)
  }

  def genXmlAttributes(
    maxAttrs: Int         = 1,
    maxAttrNameSize: Int  = 10,
    maxAttrValueSize: Int = 10
  ): Gen[List[XmlAttribute]] =
    for {
      size <- Gen.choose[Int](0, maxAttrs)
      attributesNames <-
        if (size > 0)
          Gen.listOfN(size, getNonEmptyString(maxAttrNameSize)).map(_.distinct)
        else
          Gen.const(Nil)
      values <-
        if (attributesNames.nonEmpty)
          Gen.listOfN(attributesNames.size, getNonEmptyString(maxAttrValueSize))
        else
          Gen.const(Nil)
    } yield attributesNames.zip(values).map(t => XmlAttribute(t._1, t._2))

  def getNonEmptyString(maxSize: Int = 10): Gen[String] =
    Gen.lzy(
      Gen
        .choose(1, maxSize)
        .flatMap(Gen.stringOfN(_, Gen.alphaChar))
    )
}
