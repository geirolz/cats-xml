import sbt.project

lazy val prjName                = "cats-xml"
lazy val prjPackageName         = prjName.replaceAll("[^\\p{Alpha}\\d]+", ".")
lazy val prjDescription         = "A purely functional XML library"
lazy val org                    = "com.github.geirolz"
lazy val scala213               = "2.13.9"
lazy val scala32                = "3.2.0"
lazy val supportedScalaVersions = List(scala213, scala32)

//## global project to no publish ##
val copyReadMe = taskKey[Unit]("Copy generated README to main folder.")
lazy val `cats-xml`: Project = project
  .in(file("."))
  .settings(
    inThisBuild(
      List(
        organization := org,
        homepage     := Some(url(s"https://github.com/geirolz/$prjName")),
        licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
        developers := List(
          Developer(
            "DavidGeirola",
            "David Geirola",
            "david.geirola@gmail.com",
            url("https://github.com/geirolz")
          )
        )
      )
    )
  )
  .settings(baseSettings)
  .settings(noPublishSettings)
  .settings(
    crossScalaVersions := Nil
  )
  .settings(
    copyReadMe := IO.copyFile(file("docs/compiled/README.md"), file("README.md"))
  )
  .aggregate(core, docs, metrics, utils, generic, effect, scalaxml, xpath)

lazy val docs: Project =
  project
    .in(file("docs"))
    .enablePlugins(MdocPlugin)
    .dependsOn(core, effect, generic, scalaxml)
    .settings(
      baseSettings,
      noPublishSettings,
      libraryDependencies ++= Seq(
        ProjectDependencies.Docs.dedicated
      ).flatten,
      // config
      scalacOptions --= Seq("-Werror", "-Xfatal-warnings"),
      mdocIn  := file("docs/source"),
      mdocOut := file("docs/compiled"),
      mdocVariables := Map(
        "VERSION" -> previousStableVersion.value.getOrElse("<version>"),
        "DOC_OUT" -> mdocOut.value.getPath
      )
    )

lazy val utils: Project =
  buildModule(
    prjModuleName = "utils",
    toPublish     = false,
    folder        = "."
  ).settings(
    libraryDependencies ++= ProjectDependencies.Utils.dedicated
  )

lazy val core: Project =
  buildModule(
    prjModuleName = "core",
    toPublish     = true,
    folder        = "."
  ).dependsOn(utils)

lazy val metrics: Project =
  buildModule(
    prjModuleName = "metrics",
    toPublish     = false,
    folder        = "."
  ).dependsOn(core, utils, effect, scalaxml, generic)
    .settings(
      libraryDependencies ++= ProjectDependencies.Metrics.dedicated
    )

// modules
lazy val generic: Project =
  buildModule(
    prjModuleName = "generic",
    toPublish     = true,
    folder        = "modules"
  ).dependsOn(core, utils)
    .settings(
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) => ProjectDependencies.Generic.scala2
          case Some((3, _)) => ProjectDependencies.Generic.scala3
          case _            => Nil
        }
      }
    )

lazy val effect: Project =
  buildModule(
    prjModuleName = "effect",
    toPublish     = true,
    folder        = "modules"
  ).dependsOn(core, utils)
    .settings(
      libraryDependencies ++= ProjectDependencies.Effect.dedicated
    )

lazy val scalaxml: Project =
  buildModule(
    prjModuleName = "standard",
    toPublish     = true,
    folder        = "modules"
  ).dependsOn(core, utils)
    .settings(
      libraryDependencies ++= ProjectDependencies.Standard.dedicated
    )

lazy val xpath: Project =
  buildModule(
    prjModuleName = "xpath",
    toPublish     = true,
    folder        = "modules"
  ).dependsOn(core, utils)
    .settings(
      libraryDependencies ++= ProjectDependencies.Xpath.dedicated
    )

//=============================== MODULES UTILS ===============================
def buildModule(prjModuleName: String, toPublish: Boolean, folder: String): Project = {
  val keys       = prjModuleName.split("-")
  val id         = keys.reduce(_ + _.capitalize)
  val docName    = keys.mkString(" ")
  val prjFile    = file(s"$folder/$prjModuleName")
  val docNameStr = s"$prjName $docName"

  Project(id, prjFile)
    .settings(
      description    := moduleName.value,
      moduleName     := s"$prjName-$prjModuleName",
      name           := s"$prjName $docName",
      publish / skip := !toPublish,
      baseSettings
    )
}

//=============================== SETTINGS ===============================
lazy val noPublishSettings: Seq[Def.Setting[_]] = Seq(
  publish         := {},
  publishLocal    := {},
  publishArtifact := false,
  publish / skip  := true
)

lazy val baseSettings: Seq[Def.Setting[_]] = Seq(
  // project
  name             := prjName,
  description      := prjDescription,
  organization     := org,
//  idePackagePrefix := Some(prjPackageName),
  // scala
  crossScalaVersions := supportedScalaVersions,
  scalaVersion       := supportedScalaVersions.head,
  scalacOptions ++= scalacSettings(scalaVersion.value),
  versionScheme := Some("early-semver"),
  // dependencies
  resolvers ++= ProjectResolvers.all,
  libraryDependencies ++= ProjectDependencies.common ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => ProjectDependencies.Plugins.compilerPluginsFor2_13
      case Some((3, _))  => ProjectDependencies.Plugins.compilerPluginsFor3
      case _             => Nil
    }
  },
  // fmt
  scalafmtOnCompile := true
)

def scalacSettings(scalaVersion: String): Seq[String] =
  Seq(
    //    "-Xlog-implicits",
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-language:dynamics"
  ) ++ {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _)) =>
        Seq(
          "-Ykind-projector",
          "-explain-types", // Explain type errors in more detail.
          "-Xfatal-warnings" // Fail the compilation if there are any warnings.
        )
      case Some((2, 13)) =>
        Seq(
          "-explaintypes", // Explain type errors in more detail.
          "-unchecked", // Enable additional warnings where generated code depends on assumptions.
          "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
          "-Xfatal-warnings", // Fail the compilation if there are any warnings.
          "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
          "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
          "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
          "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
          "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
          "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
          "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
          "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
          "-Xlint:option-implicit", // Option.apply used implicit view.
          "-Xlint:package-object-classes", // Class or object defined in package object.
          "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
          "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
          "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
          "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
          "-Ywarn-dead-code", // Warn when dead code is identified.
          "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
          "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
          "-Ywarn-numeric-widen", // Warn when numerics are widened.
          "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
          "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
          "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
          "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
          "-Ywarn-unused:locals", // Warn if a local definition is unused.
          "-Ywarn-unused:explicits", // Warn if a explicit value parameter is unused.
          "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
          "-Ywarn-unused:privates", // Warn if a private member is unused.
          "-Ywarn-macros:after", // Tells the compiler to make the unused checks after macro expansion
          "-Xsource:3",
          "-P:kind-projector:underscore-placeholders"
        )
      case _ => Nil
    }
  }

//=============================== ALIASES ===============================
addCommandAlias("check", "scalafmtAll;clean;coverage;test;coverageAggregate")
addCommandAlias("gen-doc", "mdoc;copyReadMe;")
addCommandAlias("coverage-test", "coverage;test;coverageReport")
