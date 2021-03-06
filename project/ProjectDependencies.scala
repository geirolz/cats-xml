import sbt.{CrossVersion, _}

/** cats-xml Created by geirolz on 30/07/2019.
  *
  * @author
  *   geirolz
  */
object ProjectDependencies {

  lazy val common: Seq[ModuleID] = Seq(
    // SCALA
    "org.typelevel" %% "cats-core" % "2.8.0" cross CrossVersion.binary,
//    "org.typelevel" %% "mouse"     % "1.0.10",
//    "org.scala-lang" % "scala-compiler" % "2.13.8",
    // TEST
    "org.scalameta"  %% "munit"            % "0.7.29" % Test,
    "org.scalameta"  %% "munit-scalacheck" % "0.7.29" % Test,
    "org.typelevel"  %% "cats-laws"        % "2.8.0"  % Test,
    "org.typelevel"  %% "discipline-munit" % "1.0.9"  % Test,
    "org.scalacheck" %% "scalacheck"       % "1.16.0" % Test cross CrossVersion.binary
  )

  object Docs {
    val dedicated: Seq[ModuleID] = Nil
  }

  object Metrics {
    val dedicated: Seq[ModuleID] = Nil
  }

  object Generic {
    val scala2: Seq[ModuleID] = Seq(
      "com.softwaremill.magnolia1_2" %% "magnolia"      % "1.1.2",
      "org.scala-lang"                % "scala-reflect" % "2.13.8",
      "com.chuusai"                  %% "shapeless"     % "2.3.8"
    )
    val scala3: Seq[ModuleID] = Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.1"
    )
  }

  object Effect {
    val dedicated: Seq[ModuleID] = Seq(
      "org.typelevel" %% "cats-effect"         % "3.3.14" cross CrossVersion.binary,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
    )
  }

  object Standard {
    val dedicated: Seq[ModuleID] = Seq(
      "org.scala-lang.modules" %% "scala-xml" % "2.1.0" cross CrossVersion.binary
    )
  }

  object Plugins {
    lazy val compilerPluginsFor2_13: Seq[ModuleID] = Seq(
      compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
    )
    lazy val compilerPluginsFor3: Seq[ModuleID] = Nil
  }
}
