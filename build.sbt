name := "sbt-breeze-expand-codegen"
organization := "org.scalanlp"
version := "0.1"
sbtPlugin := true
description := "code generation for Breeze"

scalacOptions := Seq("-deprecation", "-unchecked")

libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.13"

publishTo := {
  val nexus = "https://oss.sonatype.org"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at s"$nexus/content/repositories/snapshots")
  else
    Some("releases"  at s"$nexus/service/local/staging/deploy/maven2")
}
publishArtifact in Test := false

crossSbtVersions := Seq("0.13.18", "1.3.13", "1.4.9")
