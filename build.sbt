name := "onion-example"

version := "1.0"

scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")

libraryDependencies += "org.typelevel" %% "cats" % "0.7.2"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ypartial-unification"
) ++ unusedWarnings

lazy val unusedWarnings = Seq(
  "-Ywarn-unused",
  "-Ywarn-unused-import"
)