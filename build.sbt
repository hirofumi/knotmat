import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
import xerial.sbt.Sonatype._

lazy val knotmat =
  project
    .in(file("."))
    .settings(common)
    .settings(
      name            := "knotmat",
      skip in publish := true
    )
    .aggregate(
      core,
      xgboost
    )

lazy val core =
  project
    .in(file("modules/core"))
    .settings(common)

lazy val xgboost =
  project
    .in(file("modules/xgboost"))
    .settings(common)

lazy val common =
  Seq(
    name              := s"knotmat ${thisProject.value.id}",
    organization      := "com.github.hirofumi",
    publishMavenStyle := true,
    publishTo         := sonatypePublishTo.value,
    scalaVersion      := "2.12.4"
  ) ++ Seq(
    licenses := Seq(
      "MIT" -> url("https://opensource.org/licenses/MIT")
    ),
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepCommand("publishSigned"),
      setNextVersion,
      commitNextVersion,
      releaseStepCommand("sonatypeReleaseAll"),
      pushChanges
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-target:jvm-1.8",
      "-unchecked",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard"
    ),
    scalacOptions in (Compile, compile) ++= Seq(
      "-Xfatal-warnings",
      "-Xlint",
      "-Ywarn-unused-import"
    ),
    scalacOptions in (Compile, console) ++= Seq(
      "-Xlint:-unused"
    ),
    sonatypeProjectHosting := Some(
      GithubHosting("hirofumi", "knotmat", "hirofummy@gmail.com")
    ),
    testOptions ++= Seq(
      Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
      Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
    )
  )
