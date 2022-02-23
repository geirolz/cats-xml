import cats.xml.{Xml, XmlNode}
import cats.xml.codec.{Decoder, Encoder}
import cats.xml.cursor.CursorResult
import cats.xml.cursor.NodeCursor.Root
import cats.xml.XmlAttribute.XmlAttrStringOps

//############### PARSING from NODESEQ ###############
val n1: XmlNode = <root>TEST</root>

//############### CURSOR ###############
val node: XmlNode =
  <root>
    <foo>
      <bar>
        <root>
          <foo>
            <bar>
              <root>
                <foo>
                  <bar>
                    <roar a="1" b="2" c="3">
                      LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA
                    </roar>
                  </bar>
                </foo>
              </root>
            </bar>
          </foo>
        </root>
      </bar>
    </foo>
  </root>

node.findDeepChild("roar")
Xml.toNodeSeq(node)

val result1: CursorResult[Int] =
  Root
    .down("foo")
    .down("bar")
    .down("root")
    .down("foo")
    .down("bar")
    .down("root")
    .down("foo")
    .down("bar")
    .down("roar")
    .attr("a")
    .as[Int]
    .focus(node)


val result1: CursorResult[Int] =
  (Root \ "foo" \ "bar" \ "root" \ "foo"
    \ "bar" \ "root" \ "foo" \ "bar"
    \ "roar" attr "a").as[Int]
    .focus(node)

// ############### DECODER ###############
val tree: XmlNode = Xml.fromNodeSeq(<Foo name="TEST" age="10">100</Foo>)

val ressa = tree.findChild("foo")


case class Foo(name: Option[String], bar: Int, text: Int)
val dec: Decoder[Foo] =
  Decoder.fromCursor(c =>
    for {
      foo <- c.attr("name").as[Option[String]]
      bar <- c.attr("age").as[Int]
      text <- c.text.as[Int]
    } yield Foo(foo, bar, text)
  )

val result: Decoder.Result[Foo] = dec.decode(tree) //Valid(Foo(None,10))

//############### ENCODER ###############
val encoder: Encoder[Foo] = Encoder.of(t =>
  XmlNode("Foo")
    .withAttributes(
      "name" := t.name.get,
      "age"  := t.bar
    )
    .withText(t.text)
)


val res1 = dec.decode(tree).toOption.map(encoder.encode).get







val ORIGINAL: XmlNode = Xml.fromNodeSeq(<Test name="FOO" age="20">200</Test>)


val modifyResult = Root
  .modify(_.withText("HEEEEEY!"))
  .apply(ORIGINAL)


ORIGINAL