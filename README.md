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
libraryDependencies += "com.github.geirolz" %% "cats-xml" % "0.0.4"
```

This library is not production ready yet. There is a lot of work to do to complete it:
- ~~There are some performance issues loading xml from strings or files due the fact that there is a double
  conversion, We need to parse bytes directly into `cats-xml` classes.~~
~~- Creates macro to derive `Decoder` and `Encoder`. This is not straightforward, distinguish between a Node and an Attribute ca
  can be done in some way thinking about attributes with primitives and value classes BUT distinguish between a Node/Attribute and Text 
  is hard, probably an annotation or a custom Decoder/Encoder is required.~~ 
- ~~Reach a good code coverage with the tests (using munit)~~
- Improve documentation
- Literal macros to check XML strings at compile time

Contributions are more than welcome 💪

## Modules
- [Effect](docs/compiled/effect.md)
- [Generic](docs/compiled/generic.md)
- [Standard](docs/compiled/standard.md)
- [XPath](docs/compiled/xpath.md)

## Example
Given
```scala
case class Foo(
    foo: Option[String], 
    bar: Int, 
    text: Boolean
)
```

### Decoding
```scala
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
```scala
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
```scala
import cats.xml.XmlNode
import cats.xml.cursor.Cursor
import cats.xml.cursor.FreeCursor

val node: XmlNode =
  XmlNode("wrapper")
    .withChildren(
      XmlNode("root")
        .withChildren(
          XmlNode("foo").withText(1),
          XmlNode("baz").withText(2),
          XmlNode("bar").withText(3)
        )
    )
// node: XmlNode = <wrapper>
//  <root>
//   <foo>1</foo>
//   <baz>2</baz>
//   <bar>3</bar>
//  </root>
// </wrapper>

val fooNode: Cursor.Result[XmlNode] = node.focus(_.root.foo)
// fooNode: Cursor.Result[XmlNode] = Right(value = <foo>1</foo>)
val fooTextValue: FreeCursor.Result[Int] = node.focus(_.root.foo.text.as[Int])
// fooTextValue: FreeCursor.Result[Int] = Valid(a = 1)
```

### Modifying
```scala
import cats.xml.XmlNode
import cats.xml.cursor.NodeCursor.Root
import cats.xml.modifier.Modifier
import cats.xml.implicits.*

val node: XmlNode = XmlNode("Foo")
  .withAttributes(
    "name" := "Foo",
    "age"  := 10
  )
  .withText("ORIGINAL")
// node: XmlNode = <Foo name="Foo" age="10">ORIGINAL</Foo>
  
val result: Modifier.Result[XmlNode] = Root
  .modifyIfNode(_.withText("NEW"))
  .apply(node)  
// result: Modifier.Result[XmlNode] = Right(
//   value = <Foo name="Foo" age="10">NEW</Foo>
// )
```