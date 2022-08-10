package cats.xml.generic

final case class Configuration(
  useDefaults: Boolean,
  discriminator: Option[String]
) {

  def withDefaults: Configuration =
    copy(useDefaults = true)

  def withDiscriminator(discriminator: String): Configuration =
    copy(discriminator = Some(discriminator))
}
object Configuration {

  val default: Configuration = Configuration(
    useDefaults   = false,
    discriminator = None
  )
}
