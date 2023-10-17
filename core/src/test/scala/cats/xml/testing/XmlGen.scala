package cats.xml.testing

import cats.xml.XmlData.{XmlBigDecimal, XmlBool, XmlChar, XmlDouble, XmlFloat, XmlLong, XmlString}
import cats.xml.testing.GenUtils.getNonEmptyString
import cats.xml.*
import org.scalacheck.{Arbitrary, Gen}

object XmlGen {

  def genXmlNode(
    size: DataSize,
    maxNodeName: Int     = 10,
    maxAttrNameSize: Int = 10
  ): Gen[XmlNode] = {

    def compile(maxAttrs: Int, maxChildren: Int, maxDeep: Int): Gen[XmlNode] =
      _genXmlNode(
        maxAttrs        = maxAttrs,
        maxChildren     = maxChildren,
        maxDeep         = maxDeep,
        maxNodeName     = maxNodeName,
        maxAttrNameSize = maxAttrNameSize
      )

    size match {
      case DataSize.S =>
        compile(
          maxAttrs    = 5,
          maxChildren = 1,
          maxDeep     = 1
        )
      case DataSize.M =>
        compile(
          maxAttrs    = 5,
          maxChildren = 2,
          maxDeep     = 2
        )
      case DataSize.L =>
        compile(
          maxAttrs    = 15,
          maxChildren = 4,
          maxDeep     = 4
        )
      case DataSize.XL =>
        compile(
          maxAttrs    = 30,
          maxChildren = 6,
          maxDeep     = 6
        )
    }
  }

  private def _genXmlNode(
    maxAttrs: Int,
    maxChildren: Int,
    maxDeep: Int,
    maxNodeName: Int     = 10,
    maxAttrNameSize: Int = 10
  ): Gen[XmlNode] = {

    def genChildren: Gen[NodeContent] =
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
            .map(NodeContent.children(_))
        )
      else
        Gen.const(NodeContent.empty)

    for {
      nodeName   <- Gen.lzy(getNonEmptyString(maxNodeName))
      attributes <- Gen.lzy(genXmlAttributes(maxAttrs, maxAttrNameSize))
      content <- Gen.lzy(
        Gen.frequency(
          2  -> Gen.const(NodeContent.empty),
          18 -> Gen.lzy(genXmlData.map(NodeContent.text(_))),
          80 -> Gen.lzy(genChildren)
        )
      )
    } yield XmlNode(nodeName)
      .withAttrs(attributes)
      .withContent(content)
  }

  def genXmlAttributes(
    maxAttrs: Int        = 1,
    maxAttrNameSize: Int = 10
  ): Gen[List[XmlAttribute]] =
    for {
      size <- Gen.choose[Int](0, maxAttrs)
      attributes <-
        if (size > 0)
          Gen.listOfN(size, genAttribute(maxAttrNameSize)).map(_.distinct)
        else
          Gen.const(Nil)
    } yield attributes

  def genAttribute(
    maxAttrNameSize: Int = 10
  ): Gen[XmlAttribute] =
    for {
      name  <- XmlValidName.genXmlValidName(maxAttrNameSize)
      value <- genXmlData
    } yield XmlAttribute(name.value, value)

  def genXmlData: Gen[XmlData] =
    Gen.oneOf[XmlData](
      genXmlString,
      genXmlChar,
      genXmlBool,
      genXmlLong,
      genXmlFloat,
      genXmlDouble,
      genXmlBigDecimal
    )

  def genXmlString: Gen[XmlString] =
    Gen.asciiPrintableStr.map(Xml.string)

  def genXmlChar: Gen[XmlChar] =
    Gen.alphaNumChar.map(Xml.char)

  def genXmlBool: Gen[XmlBool] =
    Gen.oneOf(true, false).map(Xml.boolean)

  def genXmlLong: Gen[XmlLong] =
    Gen.long
      .map(Xml.long)
      .map(_.asInstanceOf[XmlLong])

  def genXmlFloat: Gen[XmlFloat] =
    Arbitrary.arbFloat.arbitrary
      .map(Xml.float)
      .map(_.asInstanceOf[XmlFloat])

  def genXmlDouble: Gen[XmlDouble] =
    Gen.double
      .map(Xml.double)
      .map(_.asInstanceOf[XmlDouble])

  def genXmlBigDecimal: Gen[XmlBigDecimal] =
    Arbitrary.arbBigDecimal.arbitrary
      .map(Xml.bigDecimal)
      .map(_.asInstanceOf[XmlBigDecimal])
}
