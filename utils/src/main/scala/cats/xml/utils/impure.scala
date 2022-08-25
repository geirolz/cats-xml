package cats.xml.utils

import scala.annotation.StaticAnnotation

/** Annotation used to mark method that are impure functions. This means that a method with this
  * annotation can have side-effects like throwing exceptions.
  *
  * When possible is better to avoid side-effects but, sometime for usability purpose you have to
  * trow exception instead of returning a monad which is more complex to use. For example to
  * validate some parameters
  *
  * Be careful with this annotation and propagate it up.
  */
final class impure extends StaticAnnotation
