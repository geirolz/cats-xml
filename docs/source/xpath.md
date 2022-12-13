# XPath support

Add XPath support.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-xpath" % "@VERSION@"
```

With this module you can create `NodeCursor`s instances using XPath.

Using `NodeCursor` companion object
```scala mdoc:nest
import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error.*
import cats.xml.xpath.implicits.*

val cursor: Either[XPathError, NodeCursor] = NodeCursor.fromXPath("/root[@id='1']")
```

Using string interpolation
```scala mdoc:nest
import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error.*
import cats.xml.xpath.implicits.*

val cursor: Either[XPathError, NodeCursor] = xpath"/root[@id='1']"
```


Full example
```scala mdoc:reset
import cats.xml.XmlNode
import cats.xml.cursor.NodeCursor
import cats.xml.xpath.error.*

import cats.implicits.*
import cats.xml.implicits.*
import cats.xml.xpath.implicits.*

val cursor: Either[XPathError, NodeCursor] = xpath"/root[@id='1']"

val data = XmlNode("wrapper").withChildren(
  XmlNode("root").withAttributes("id" := 1)
)
val result: Either[Throwable, XmlNode] =
  cursor
    .leftMapThrowable
    .flatMap(_.focus(data).leftMap(_.asException))
```

If you want you can use the xpath expression directly on a `XmlNode` focusing using that xpath
```scala mdoc:reset
import cats.implicits.*
import cats.xml.XmlNode
import cats.xml.cursor.{Cursor, NodeCursor}
import cats.xml.xpath.error.*

import cats.xml.implicits.*
import cats.xml.xpath.implicits.*

val data = XmlNode("wrapper").withChildren(
  XmlNode("root").withAttributes("id" := 1)
)
val result: Cursor.Result[XmlNode] = data.xpath("/root[@id='1']")
```