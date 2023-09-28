import cats.xml._
import cats.xml.implicits._
import cats.xml.modifier._

val data = xml"""
     <wrapper>
         <root>
           <foo>
             <bar>
               <baz>
                 <value>1</value>
               </baz>
             </bar>
           </foo>
         </root>
       </wrapper>"""

val result: Modifier.Result[XmlNode] =
  data.modify(_.root.foo.bar.baz.value.modifyNode(_.withText(2)))
