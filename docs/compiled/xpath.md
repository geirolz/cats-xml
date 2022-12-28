# XPath support

Add XPath support.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-xpath" % "0.0.7"
```

With this module you can create `NodeCursor`s instances using XPath.

Using `NodeCursor` companion object
```scala
import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error.*
import cats.xml.xpath.implicits.*

val cursor: Either[XPathError, NodeCursor] = NodeCursor.fromXPath("/root[@id='1']")
// cursor: Either[XPathError, NodeCursor] = Right(
//   value = /root[filter cats.xml.xpath.CursorBuilder$PredicateBuilder$$$Lambda$30577/0x0000000802acc678@2ed8ec0f]
// )
```

Using string interpolation
```scala
import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error.*
import cats.xml.xpath.implicits.*

val cursor: Either[XPathError, NodeCursor] = xpath"/root[@id='1']"
// cursor: Either[XPathError, NodeCursor] = Right(
//   value = /root[filter cats.xml.xpath.CursorBuilder$PredicateBuilder$$$Lambda$30577/0x0000000802acc678@49be6d00]
// )
```


Full example
```scala
import cats.xml.XmlNode
import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error.*

import cats.implicits.*
import cats.xml.implicits.*
import cats.xml.xpath.implicits.*

val cursor: Either[XPathError, NodeCursor] = xpath"/root[@id='1']"
// cursor: Either[XPathError, NodeCursor] = Right(
//   value = /root[filter cats.xml.xpath.CursorBuilder$PredicateBuilder$$$Lambda$30577/0x0000000802acc678@7a50f44b]
// )

val data = XmlNode("wrapper").withChildren(
  XmlNode("root").withAttributes("id" := 1)
)
// data: XmlNode.Node = <wrapper>
//  <root id="1"/>
// </wrapper>
val result: Either[Throwable, XmlNode] =
  cursor
    .leftMapThrowable
    .flatMap(_.focus(data).leftMap(_.asException))
// result: Either[Throwable, XmlNode] = Right(value = <root id="1"/>)
```

If you want you can use the xpath expression directly on a `XmlNode` focusing using that xpath
```scala
import cats.xml.XmlNode
import cats.xml.cursor.Cursor

import cats.xml.implicits.*
import cats.xml.xpath.implicits.*

val data = XmlNode("wrapper").withChildren(
  XmlNode("root").withAttributes("id" := 1)
)
// data: XmlNode.Node = <wrapper>
//  <root id="1"/>
// </wrapper>
val result: Cursor.Result[XmlNode] = data.xpath("/root[@id='1']")
// result: Cursor.Result[XmlNode] = Right(value = <root id="1"/>)
```