package cats.xml

import cats.ApplicativeThrow
import cats.xml.utils.impure
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.{Charset, StandardCharsets}
import javax.xml.parsers.{SAXParser, SAXParserFactory}
import scala.util.Try

//TODO: Rethink the design of this class to return `F[Xml]` instead of `F[XmlNode]` so, parsing XmlData too.
trait XmlParser[F[_]] {

  def parseInputStream(inputStream: InputStream): F[XmlNode]

  def parseString(text: String, charset: Charset = StandardCharsets.UTF_8): F[XmlNode] =
    parseInputStream(new ByteArrayInputStream(text.getBytes(charset)))
}
object XmlParser extends XmlParserSyntax {

  import cats.implicits.*

  private val defaultSaxParser: SAXParser = synchronized {
    val parserFactory: SAXParserFactory = SAXParserFactory.newInstance()
    parserFactory.setFeature("http://javax.xml.XMLConstants/feature/secure-processing", true)
    parserFactory.setFeature(
      "http://apache.org/xml/features/nonvalidating/load-external-dtd",
      false
    )
    parserFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    parserFactory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    parserFactory.setFeature("http://xml.org/sax/features/external-general-entities", false)
    parserFactory.setFeature("http://xml.org/sax/features/resolve-dtd-uris", false)
    parserFactory.setXIncludeAware(false)
    parserFactory.setNamespaceAware(false)
    parserFactory.newSAXParser()
  }

  def apply[F[_]](implicit parser: XmlParser[F]): XmlParser[F] = parser

  def fromSAXParser[F[_]: ApplicativeThrow](
    saxParser: SAXParser
  ): XmlParser[F] =
    (inputStream: InputStream) =>
      Try {

        var documentNode: XmlNode = null
        val handler: DefaultHandler = new DefaultHandler {

          var nodes: List[XmlNode] = Nil

          @impure
          override def startElement(
            uri: String,
            localName: String,
            qName: String,
            attributes: Attributes
          ): Unit = {

            val newNode: XmlNode.Node = XmlNode(qName)
              .withAttrs(
                (0 until attributes.getLength).map(i =>
                  XmlAttribute(
                    attributes.getLocalName(i),
                    Xml.string(attributes.getValue(i))
                  )
                )
              )

            if (nodes.isEmpty)
              documentNode = newNode
            else
              nodes.last.unsafeMute(_.appendChildren(newNode))

            nodes = nodes :+ newNode
          }

          override def endElement(uri: String, localName: String, qName: String): Unit =
            nodes = nodes.dropRight(1)

          override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
            val value = new String(ch, start, length).trim
            if (value != null & value.nonEmpty)
              nodes.last.unsafeMute(
                _.unsafeNarrowNode.withText(
                  Xml.string(value)
                )
              )
          }
        }

        synchronized {
          saxParser.parse(inputStream, handler)
          documentNode
        }
      }.liftTo[F]

  implicit def defaultXmlSAXParser[F[_]: ApplicativeThrow]: XmlParser[F] =
    fromSAXParser[F](defaultSaxParser)
}
private[xml] trait XmlParserSyntax {

  implicit class XmlParserInputStreamOps(inputStream: InputStream) {
    def parseXml[F[_]: XmlParser]: F[XmlNode] =
      XmlParser[F].parseInputStream(inputStream)
  }

  implicit class XmlParserStringOps(string: String) {
    def parseXml[F[_]: XmlParser]: F[XmlNode] =
      XmlParser[F].parseString(string)
  }

  implicit class XmlParserStringCtxOps(
    ctx: StringContext
  )(implicit
    parserTry: XmlParser[Try]
  ) {
    // TODO: use macro
    def xml(args: Any*): XmlNode = parserTry.parseString(ctx.s(args*)).get
  }
}
