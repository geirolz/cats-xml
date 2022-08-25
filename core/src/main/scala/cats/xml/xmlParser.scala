package cats.xml

import cats.MonadThrow
import cats.xml.utils.impure
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.{Charset, StandardCharsets}
import javax.xml.parsers.{SAXParser, SAXParserFactory}
import scala.util.Try

trait XmlParser[F[_]] {

  def parseInputStream(inputStream: InputStream): F[XmlNode]

  def parseString(text: String, charset: Charset = StandardCharsets.UTF_8): F[XmlNode] =
    parseInputStream(new ByteArrayInputStream(text.getBytes(charset)))
}
object XmlParser extends XmlParserInstances {
  def apply[F[_]](implicit parser: XmlParser[F]): XmlParser[F] = parser
}

private[xml] trait XmlParserInstances {

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

  implicit val xmlParserForTry: XmlParser[Try] = (inputStream: InputStream) =>
    Try {

      @impure
      var initNode: XmlNode.Node = XmlNode("TEMP_NODE")
      val handler: DefaultHandler = new DefaultHandler {

        var depth: Int           = 0
        var nodes: List[XmlNode] = Nil

        @impure
        override def startElement(
          uri: String,
          localName: String,
          qName: String,
          attributes: Attributes
        ): Unit = {

          val newNode: XmlNode.Node = XmlNode(qName)
            .withAttributes(
              (0 until attributes.getLength).map(i =>
                XmlAttribute(
                  attributes.getLocalName(i),
                  attributes.getValue(i)
                )
              )
            )

          val node: XmlNode = if (depth == 0) {
            initNode = newNode
            newNode
          } else {
            nodes(depth - 1).unsafeMute(_.appendChild(newNode))
            newNode
          }

          nodes = nodes :+ node
          depth = depth + 1
        }

        override def endElement(uri: String, localName: String, qName: String): Unit = {
          depth = depth - 1
          nodes = nodes.drop(depth);
        }

        override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
          val value = new String(ch, start, length).trim
          if (value != null & value.nonEmpty)
            nodes(depth - 1).unsafeMute(_.unsafeNarrowNode.withText(value))
        }
      }

      synchronized {
        defaultSaxParser.parse(inputStream, handler)
        initNode
      }
    }

  implicit def xmlParserOfMonadThrow[F[_]: MonadThrow]: XmlParser[F] =
    (inputStream: InputStream) => xmlParserForTry.parseInputStream(inputStream).liftTo[F]
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
