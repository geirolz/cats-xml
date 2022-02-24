import sbt.{Resolver, _}
import sbt.librarymanagement.MavenRepository

/** cats-xml Created by geirolz on 26/07/2019.
  *
  * @author
  *   geirolz
  */
object ProjectResolvers {

  lazy val all: Seq[MavenRepository] = Seq(
    Resolver.sonatypeRepo("public"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    "Maven repo1" at "https://repo1.maven.org/",
    "Maven repo2" at "https://mvnrepository.com/artifact"
  )
}
