import cats.xml.XmlNode
import cats.xml.codec.{Decoder, Encoder}
import cats.xml.implicits.*
//
////############### PARSING from NODESEQ ###############
//val n1: XmlNode = <root>TEST</root>
//
////############### CURSOR ###############
//val node: XmlNode =
//  <root>
//    <foo>
//      <bar>
//        <root>
//          <foo>
//            <bar>
//              <root>
//                <foo>
//                  <bar>
//                    <roar a="1" b="2" c="3">
//                      LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA
//                    </roar>
//                  </bar>
//                </foo>
//              </root>
//            </bar>
//          </foo>
//        </root>
//      </bar>
//    </foo>
//  </root>
//
//node.findDeepChild("roar")
//Xml.toNodeSeq(node)
//
//val result1: CursorResult[Int] =
//  Root
//    .down("foo")
//    .down("bar")
//    .down("root")
//    .down("foo")
//    .down("bar")
//    .down("root")
//    .down("foo")
//    .down("bar")
//    .down("roar")
//    .attr("a")
//    .as[Int]
//    .focus(node)
//
//
//val result1: CursorResult[Int] =
//  (Root \ "foo" \ "bar" \ "root" \ "foo"
//    \ "bar" \ "root" \ "foo" \ "bar"
//    \ "roar" attr "bb").as[Int]
//    .focus(node)
//
//// ############### DECODER ###############
val tree: XmlNode =
  XmlNode("Foo")
    .withAttributes("name" := "TEST")
    .withAttributes("age" := "10")
    .withText("1")
//
//val ressa = tree.findChild("foo")
//

case class Foo(name: Option[String], bar: Int, text: Boolean)
val dec: Decoder[Foo] =
  Decoder.fromCursor(c =>
    for {
      foo <- c.attr("name1").as[Option[String]]
      bar <- c.attr("age1").as[Int]
      bar1 <- c.attr("age2").as[Int]
      text <- c.text.as[Boolean]
    } yield Foo(foo, bar + bar1, text)
  )

val result: Decoder.Result[Foo] = dec.decode(tree) //Valid(Foo(None,10))
result.toString

//############### ENCODER ###############
val encoder: Encoder[Foo] = Encoder.of(t =>
  XmlNode("Foo")
    .withAttributes(
      "name" := t.name.get,
      "age"  := t.bar
    )
    .withText(t.text)
)


//val res1 = dec.decode(tree).toOption.map(encoder.encode).get
//

//
//
//
//
//
//val ORIGINAL: XmlNode = Xml.fromNodeSeq(<Test name="FOO" age="20">200</Test>)
//
//
//val modifyResult = Root
//  .modify(_.withText("HEEEEEY!"))
//  .apply(ORIGINAL)
//
//
//ORIGINAL