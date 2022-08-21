package cats.xml.generic

final case class Configuration(
  useDefaults: Boolean,
  unwrapValueClasses: Boolean
//  discriminator: Option[String]
) {

  def withDefaults: Configuration =
    copy(useDefaults = true)

  def withoutUnwrapValueClasses: Configuration =
    copy(unwrapValueClasses = false)

//  def withDiscriminator(discriminator: String): Configuration =
//    copy(discriminator = Some(discriminator))
}
object Configuration {

  val default: Configuration = Configuration(
    useDefaults        = false,
    unwrapValueClasses = true
//    discriminator = None
  )
}
