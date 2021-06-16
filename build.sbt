name := "retirement_calculator"

version := "0.1"

scalaVersion := "2.12.14"

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

mainClass in Compile := Some("retcalc.SimulatePlanApp")

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

scalacOptions += "-Ypartial-unification"
