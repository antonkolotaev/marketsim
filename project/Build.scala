import sbt.Keys._
import sbt._

object BuildSettings {
    val ScalaVersion = "2.11.7"
    val buildSettings = Defaults.coreDefaultSettings ++ Seq(
        scalacOptions ++= Seq(),
        scalaVersion := ScalaVersion,
        // Sonatype OSS deployment
        addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
    )
}
object MacMemoBuild extends Build {
    import BuildSettings._
    lazy val macros: Project = Project(
        "macros",
        file("macros"),
        settings = buildSettings ++ Seq(
            libraryDependencies ++= Seq(
                "org.scala-lang" % "scala-reflect" % ScalaVersion,
                "com.google.guava" % "guava" % "13.0.1",
                "com.google.code.findbugs" % "jsr305" % "1.3.+",
                "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
                "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
            ),
            parallelExecution in Test := false,
            scalacOptions := Seq("-feature", "-deprecation", "-Xlog-implicits")
        )
    )
    // Enabling debug project-wide. Can't find a better way to pass options to scalac.
    System.setProperty("macmemo.debug", "true")
}