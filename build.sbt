name := "scaladoc_test"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")

libraryDependencies += "org.scalameta" %% "scalameta" % "1.6.0"
libraryDependencies += "org.scalameta" %% "contrib" % "1.6.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
