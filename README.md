# cats-xml

[![Build Status](https://github.com/geirolz/cats-xml/actions/workflows/cicd.yml/badge.svg)](https://github.com/geirolz/cats-xml/actions)
[![codecov](https://img.shields.io/codecov/c/github/geirolz/cats-xml)](https://codecov.io/gh/geirolz/cats-xml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/3101ec45f0114ad0abde91181c8c238c)](https://www.codacy.com/gh/geirolz/cats-xml/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=geirolz/cats-xml&amp;utm_campaign=Badge_Grade)
[![Sonatype Nexus (Releases)](https://img.shields.io/nexus/r/com.github.geirolz/cats-xml-core_2.13?server=https%3A%2F%2Foss.sonatype.org)](https://mvnrepository.com/artifact/com.github.geirolz/cats-xml-core)
[![javadoc.io](https://javadoc.io/badge2/com.github.geirolz/cats-xml-core_2.13/javadoc.io.svg)](https://javadoc.io/doc/com.github.geirolz/cats-xml-core_2.13)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
[![GitHub license](https://img.shields.io/github/license/geirolz/cats-xml)](https://github.com/geirolz/cats-xml/blob/master/LICENSE)

A functional library to work with XML in Scala using cats core.

This library is not production ready yet. There is a lot of work to do to complete it:
- There are some performance issues loading xml from strings or files due the fact that there is a double
  conversion, We need to parse bytes directly into `cats-xml` classes.
- Creates macro to derive `Decoder` and `Encoder`. This is not straightforward, distinguish between a Node and an Attribute ca
  can be done in some way thinking about attributes with primitives and value classes BUT distinguish between a Node/Attribute and Text 
  is hard, probably an annotation or a custom Decoder/Encoder is required. 
- Reach a good code coverage with the tests (using munit)
- Improve documentation


Given `Foo` class 
```scala
case class Foo(
    foo: Option[String], 
    bar: Int, 
    text: Boolean
)
```

### Decoder
```scala
import cats.xml.codec.Decoder
import cats.xml.implicits.*

val decoder: Decoder[Foo] =
  Decoder.fromCursor(c =>
    for {
      foo <- c.attr("name").as[Option[String]]
      bar <- c.attr("bar").as[Int]
      text <- c.text.as[Boolean]
    } yield Foo(foo, bar, text)
  )
// decoder: Decoder[Foo] = cats.xml.codec.Decoder$$$Lambda$22821/1363329207@5d576366
```


### Encoder
```scala
import cats.xml.XmlNode
import cats.xml.codec.Encoder

val encoder: Encoder[Foo] = Encoder.of(t =>
  XmlNode("Foo")
    .withAttributes(
      "foo"  := t.foo.getOrElse("ERROR"),
      "bar"  := t.bar,
    )
    .withText(t.text)
)
// encoder: Encoder[Foo] = cats.xml.codec.Encoder$$$Lambda$22842/74229238@1389b63d
```