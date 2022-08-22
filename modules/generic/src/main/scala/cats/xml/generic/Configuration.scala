package cats.xml.generic

import cats.Show

final case class Configuration(
  useDefaults: Boolean,
  unwrapValueClasses: Boolean,
  discriminatorAttrKey: Option[String]
) {

  def withDefaults: Configuration =
    copy(useDefaults = true)

  def withoutUnwrapValueClasses: Configuration =
    copy(unwrapValueClasses = false)

  def withDiscriminatorAttrKey(discriminator: String): Configuration = {
    require(discriminator != null && discriminator.nonEmpty)
    copy(discriminatorAttrKey = Some(discriminator))
  }

  override def toString: String = Show[Configuration].show(this)
}
object Configuration {

  val default: Configuration = Configuration(
    useDefaults          = false,
    unwrapValueClasses   = true,
    discriminatorAttrKey = None
  )

  implicit val showConfig: Show[Configuration] = Show.show(c => s"""
      |Configuration(
      | useDefaults        = ${c.useDefaults},
      | unwrapValueClasses = ${c.unwrapValueClasses},
      | discriminatorAttrKey  = ${c.discriminatorAttrKey}
      |)""".stripMargin)
}
