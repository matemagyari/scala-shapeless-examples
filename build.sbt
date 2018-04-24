name := "scala-shapeless-examples"

organization := "home"

version := "0.1-SNAPSHOT"

scalaOrganization := "org.typelevel"

scalaVersion := "2.12.4"

libraryDependencies ++= {
  Seq(
    "com.chuusai" %% "shapeless" % "2.3.3",
    // Test dependencies
    "org.scalatest" %% "scalatest" % "3.0.1"
  )
}
