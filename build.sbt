fork in Global := true
cancelable in Global := true

val scala3Version = "3.1.3"
lazy val scalox = project
  .in(file("."))
  .settings(
    name := "scalox",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    scalacOptions ++= Seq(
      "-Ysafe-init",
      "-Yexplicit-nulls",
    )
  )
