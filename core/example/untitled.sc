import cats.xml.XmlNode
import cats.xml.cursor.NodeCursor.Root
import cats.xml.implicits._
import cats.xml.modifier.Modifier

////
//////############### PARSING from NODESEQ ###############
////val n1: XmlNode = <root>TEST</root>
////
//////############### CURSOR ###############
//val node: XmlNode =
//  xml"""<root>
//         <foo>
//           <bar>
//             <root>
//               <foo>
//                 <bar>
//                   <root>
//                     <foo>
//                       <bar>
//                         <roar a="1" b="2" c="3">
//                           LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA
//                         </roar>
//                       </bar>
//                     </foo>
//                   </root>
//                 </bar>
//               </foo>
//             </root>
//           </bar>
//         </foo>
//       </root>"""

//node.findDeepChild("roar")

val group: XmlNode = XmlNode.group(
  xml"""
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
         </foo>"""
//  xml"<Foo a='3'/>",
)
//val result1: FreeCursor.Result[Int] =
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
//    .focus(group)

val result1: Modifier.Result[XmlNode] =
  Root
    .down("bar")
    .down("root")
    .down("foo")
    .down("bar")
    .down("root")
    .down("foo")
    .down("bar")
    .down("roar")
    .attr("a")
    .set("TEST")
    .apply(group)

////
////
////val result1: CursorResult[Int] =
////  (Root \ "foo" \ "bar" \ "root" \ "foo"
////    \ "bar" \ "root" \ "foo" \ "bar"
////    \ "roar" attr "bb").as[Int]
////    .focus(node)
////
////// ############### DECODER ###############
//val tree: XmlNode =
//  XmlNode("Foo")
//    .withAttributes("name" := "TEST")
//    .withAttributes("age" := "10")
//    .withText("1")
////
////val ressa = tree.findChild("foo")
////
//
//case class Foo(name: Option[String], bar: Int, text: Boolean)
//val dec: Decoder[Foo] =
//  Decoder.fromCursor(c =>
//    for {
//      foo <- c.attr("name1").as[Option[String]]
//      bar <- c.attr("age").as[Int]
//      _ <- c.attr("age2").as[Int]
//      _ <- c.attr("age3").as[Int]
//      text <- c.text.as[Boolean]
//    } yield Foo(foo, bar, text)
//  )
//
//import cats.implicits._
//
//val dec: Decoder[Foo] =
//  Decoder.fromCursor(c =>
//    (
//      c.attr("name1").as[Option[String]],
//      c.attr("age").as[Int],
//      c.attr("age2").as[Boolean]
//    ).mapN(Foo)
//  )
//
//val result: Decoder.Result[Foo] = dec.decode(tree) //Valid(Foo(None,10))
//result.toString
////
////############### ENCODER ###############
//val encoder: Encoder[Foo] = Encoder.of(t =>
//  XmlNode("Foo")
//    .withAttributes(
//      "name" := t.name.get,
//      "age"  := t.bar
//    )
//    .withText(t.text)
//)

//val res1 = dec.decode(tree).toOption.map(encoder.encode).get
//

//
//
//
//
//
//
//
//
//
//val modifyResult = Root
//  .modifyNode(_.withText("HEEEEEY!"))
//  .apply(ORIGINAL)

//
//ORIGINAL
