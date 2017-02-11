name := "scaladoc_test"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")

libraryDependencies += "org.scalameta" %% "scalameta" % "1.6.0-671"
libraryDependencies += "org.scalameta" %% "contrib" % "1.6.0-671"
