val scala3Version = "3.1.3"
lazy val scalox = (project in file("."))
  .settings(
    name := "scalox",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-feature",
      "-Ysafe-init",
      "-Yexplicit-nulls",
      "-language:implicitConversions",
    ),
  )
