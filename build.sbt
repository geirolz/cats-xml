import sbt.project

val prjName = "cats-xml"
val org     = "com.geirolz"

//## global project to no publish ##
val copyReadMe = taskKey[Unit]("Copy generated README to main folder.")
lazy val root: Project = project
  .in(file("."))
  .aggregate(docs, core, generic, catsEffect, scalaXml)
  .settings(allSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    name        := prjName,
    description := "A purely functional XML library",
    copyReadMe  := IO.copyFile(file("docs/compiled/README.md"), file("README.md")),
    (Compile / compile) := (Compile / compile)
      .dependsOn(copyReadMe.toTask.dependsOn((docs / mdoc).toTask("")))
      .value
  )

lazy val docs: Project = {
  project
    .in(file("docs"))
    .dependsOn(core)
    .settings(allSettings: _*)
    .settings(noPublishSettings: _*)
    .enablePlugins(MdocPlugin)
    .settings(
      libraryDependencies ++= Seq(
        ProjectDependencies.Docs.dedicated
      ).flatten
    )
    .settings(
      // task
      // config
      scalacOptions --= Seq("-Werror", "-Xfatal-warnings"),
      mdocIn  := file("docs/source"),
      mdocOut := file("docs/compiled"),
      mdocVariables := Map(
        "VERSION" -> version.value,
        "DOC_OUT" -> mdocOut.value.getPath
      )
    )
}

lazy val core: Project =
  buildModule(
    prjModuleName = "core",
    toPublish     = true,
    folder        = "."
  )

lazy val generic: Project =
  buildModule(
    prjModuleName = "generic",
    toPublish     = false, // TODO ENABLE ONCE READY
    folder        = "modules"
  ).dependsOn(core)
    .settings(
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) => ProjectDependencies.Generic.scala2
          case Some((3, _)) => ProjectDependencies.Generic.scala3
          case _            => Nil
        }
      }
    )

lazy val catsEffect: Project =
  buildModule(
    prjModuleName = "cats-effect",
    toPublish     = true,
    folder        = "modules"
  ).dependsOn(core)
    .settings(
      libraryDependencies ++= ProjectDependencies.CatsEffect.dedicated
    )

lazy val scalaXml: Project =
  buildModule(
    prjModuleName = "scala-xml",
    toPublish     = true,
    folder        = "modules"
  ).dependsOn(core)
    .settings(
      libraryDependencies ++= ProjectDependencies.ScalaXml.dedicated
    )

//=============================== MODULES UTILS ===============================
def buildModule(
  prjModuleName: String,
  toPublish: Boolean,
  folder: String
): Project =
  Project(prjModuleName, file(s"$folder/$prjModuleName"))
    .settings(
      moduleName     := s"$prjName-$prjModuleName",
      description    := moduleName.value,
      organization   := org,
      publish / skip := !toPublish
    )
    .settings(allSettings: _*)

//=============================== SETTINGS ===============================
lazy val noPublishSettings: Seq[Def.Setting[_]] = Seq(
  publish         := {},
  publishLocal    := {},
  publishArtifact := false,
  publish / skip  := true
)

lazy val allSettings: Seq[Def.Setting[_]] = Seq(
  // scala
  crossScalaVersions := List("2.13.8", "3.1.0"),
  scalaVersion       := crossScalaVersions.value.head,
  scalacOptions ++= scalacSettings(scalaVersion.value),
  // dependencies
  resolvers ++= ProjectResolvers.all,
  libraryDependencies ++=
    Seq(
      ProjectDependencies.common,
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => ProjectDependencies.Plugins.compilerPluginsFor2_13
        case Some((3, _))  => ProjectDependencies.Plugins.compilerPluginsFor3
        case _             => Nil
      }
    ).flatten,
  // fmt
  scalafmtOnCompile := true
)

def scalacSettings(scalaVersion: String): Seq[String] =
  Seq(
    Seq(
//    "-Xlog-implicits",
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-encoding",
      "utf-8", // Specify character encoding used by source files.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros", // Allow macro definition (besides implementation and application)
      "-language:higherKinds", // Allow higher-kinded types
      "-language:implicitConversions" // Allow definition of implicit functions called views
    ),
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _)) =>
        Seq(
//          "-Ykind-projector",
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
          "-Xsource:3"
        )
      case _ => Nil
    }
  ).flatten
