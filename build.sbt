name := "sbt-breeze-expand-codegen"
organization := "org.scalanlp"
version := "0.1.6-SNAPSHOT"
sbtPlugin := true
description := "code generation for Breeze"

scalacOptions := Seq("-deprecation", "-unchecked")

libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.13"

publishMavenStyle := true
publishTo := sonatypePublishTo.value
publishArtifact in Test := false

crossSbtVersions := Seq("1.3.13", "1.4.9", "0.13.18")

// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName := "org.scalanlp"

// To sync with Maven central, you need to supply the following information:
publishMavenStyle := true

// Open-source license of your choice
licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

// Where is the source code hosted: GitHub or GitLab?
import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("dlwh", "sbt-breeze-expand-codegen", "david.lw.hall@gmail.com"))