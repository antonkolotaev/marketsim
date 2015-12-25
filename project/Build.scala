import sbt.Keys._
import sbt._

object BuildSettings {
    val paradiseVersion = "2.1.0-M5"
    val buildSettings = Defaults.defaultSettings ++ Seq(
        organization := "org.scalamacros",
        version := "1.0.0",
        scalaVersion := "2.11.7",
        crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7"),
        resolvers += Resolver.sonatypeRepo("snapshots"),
        resolvers += Resolver.sonatypeRepo("releases"),
        scalacOptions ++= Seq("-feature", "-deprecation"),
        addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
    )
}

object MacMemoBuild extends Build {
    import BuildSettings._
    lazy val macros: Project = Project(
        "macros",
        file("macros"),
        settings = buildSettings ++ Seq(
            libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
            libraryDependencies := {
                CrossVersion.partialVersion(scalaVersion.value) match {
                    // if Scala 2.11+ is used, quasiquotes are available in the standard distribution
                    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
                        libraryDependencies.value
                    // in Scala 2.10, quasiquotes are provided by macro paradise
                    case Some(_) =>
                        libraryDependencies.value ++ Seq(
                            compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
                            "org.scalamacros" %% "quasiquotes" % "2.1.0-M5" cross CrossVersion.binary)
                }
            },
            libraryDependencies ++= Seq(
                "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
                "com.google.guava" % "guava" % "13.0.1",
                "com.google.code.findbugs" % "jsr305" % "1.3.+"
            ),
            parallelExecution in Test := false
        )
    )

    lazy val core = Project(
        "core",
        file("core"),
        settings = buildSettings ++ Seq(
            libraryDependencies ++= Seq(
                //"org.scala-lang" % "scala-reflect" % ScalaVersion,
                "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
                "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
            ),
            parallelExecution in Test := false,
            scalacOptions := Seq("-feature", "-language:implicitConversions"))
    ).dependsOn(macros)

    lazy val generic = Project(
        "generic",
        file("generic"),
        settings = buildSettings ++ Seq(
            libraryDependencies ++= Seq(
                //"org.scala-lang" % "scala-reflect" % ScalaVersion,
                "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
                "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
            ),
            parallelExecution in Test := false,
            scalacOptions := Seq("-feature", "-deprecation", "-Xlog-implicits"))
    ).dependsOn(macros, core)


    // Enabling debug project-wide. Can't find a better way to pass options to scalac.
    //System.setProperty("macmemo.debug", "true")
}