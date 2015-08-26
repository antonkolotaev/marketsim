name := "marketsim"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"

scalacOptions += "-Xlog-implicits"
    