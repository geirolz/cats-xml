# cats-xml

[![Build Status](https://github.com/geirolz/cats-xml/actions/workflows/cicd.yml/badge.svg)](https://github.com/geirolz/cats-xml/actions)
[![codecov](https://img.shields.io/codecov/c/github/geirolz/cats-xml)](https://codecov.io/gh/geirolz/cats-xml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/3101ec45f0114ad0abde91181c8c238c)](https://www.codacy.com/gh/geirolz/cats-xml/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=geirolz/cats-xml&amp;utm_campaign=Badge_Grade)
[![Sonatype Nexus (Releases)](https://img.shields.io/nexus/r/com.github.geirolz/cats-xml-core_2.13?server=https%3A%2F%2Foss.sonatype.org)](https://mvnrepository.com/artifact/com.github.geirolz/cats-xml-core)
[![javadoc.io](https://javadoc.io/badge2/com.github.geirolz/cats-xml-core_2.13/javadoc.io.svg)](https://javadoc.io/doc/com.github.geirolz/cats-xml-core_2.13)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
[![GitHub license](https://img.shields.io/github/license/geirolz/cats-xml)](https://github.com/geirolz/cats-xml/blob/master/LICENSE)

A functional library to work with XML in Scala using cats core.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml" % "@VERSION@"
```

This library is not production ready yet. There is a lot of work to do to complete it:
- [X] Macros to derive `Encoder` and `Decoder` for Scala 2
- [X] Reach a good code coverage with the tests (using munit) above 60%
- [X] Support XPath
- [X] `Decoder` and `Encoder` for primitives with error accumulating
- [X] Good error handling and messaging 
- [X] Integration with standard scala xml library
- [X] Integration with cats-effect to load files effectfully
- [ ] Macros to derive `Encoder` and `Decoder` for Scala 2
- [ ] Performance benchmarks
- [ ] Integration with Tapir and Http4s
- [ ] Literal macros to check XML strings at compile time

Contributions are more than welcome 💪

## Modules
- [Effect](@DOC_OUT@/effect.md)
- [Generic (scala 2 only so far)](@DOC_OUT@/generic.md)
- [Standard](@DOC_OUT@/standard.md)
- [XPath](@DOC_OUT@/xpath.md)

## Example
Given
```scala mdoc:reset:silent
case class Foo(
    foo: Option[String], 
    bar: Int, 
    text: Boolean
)
```

### Decoding
```scala mdoc:nest:silent
import cats.xml.codec.Decoder
import cats.xml.implicits.*
import cats.implicits.*

val decoder: Decoder[Foo] =
  Decoder.fromCursor(c =>
    (
      c.attr("name").as[Option[String]],
      c.attr("bar").as[Int],
      c.text.as[Boolean]
    ).mapN(Foo.apply)
  )
```

### Encoding
```scala mdoc:nest:silent
import cats.xml.XmlNode
import cats.xml.codec.Encoder

val encoder: Encoder[Foo] = Encoder.of(t =>
  XmlNode("Foo")
    .withAttributes(
      "foo"  := t.foo.getOrElse("ERROR"),
      "bar"  := t.bar
    )
    .withText(t.text)
)
```

### Navigating
```scala mdoc:reset
import cats.xml.Xml
import cats.xml.XmlNode
import cats.xml.cursor.Cursor
import cats.xml.cursor.FreeCursor
import scala.util.Try

val node = Xml.fromString[Try](
  """<wrapper>
    |    <root>
    |        <foo>1</foo>
    |        <bar>2</bar>
    |        <baz>2</baz>
    |    </root>
    |</wrapper>""".stripMargin).get

val fooNode: Cursor.Result[XmlNode] = node.focus(_.root.foo)
val fooTextValue: FreeCursor.Result[Int] = node.focus(_.root.foo.text.as[Int])
```

### Modifying
```scala mdoc:reset
import cats.xml.Xml
import cats.xml.XmlNode
import cats.xml.modifier.Modifier
import scala.util.Try

val data = Xml.fromString[Try](
  """<wrapper>
    |  <root>
    |    <foo>
    |      <bar>
    |        <baz>
    |          <value>1</value>
    |        </baz>
    |      </bar>
    |    </foo>
    |  </root>
    |</wrapper>""".stripMargin).get

val result: Modifier.Result[XmlNode] = node.modify(_.root.foo.baz.bar.value.modifyIfNode(_.withText(2)))
```