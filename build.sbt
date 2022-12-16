ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2022",
    libraryDependencies ++= List(
      "org.scodec"    %% "scodec-bits"       % "1.1.34",
      "org.typelevel" %% "cats-parse"        % "0.3.8",
      "org.typelevel" %% "cats-effect-std"   % "3.4.2",
      "org.typelevel" %% "cats-effect"       % "3.4.2",
      "co.fs2"        %% "fs2-core"          % "3.4.0",
      "co.fs2"        %% "fs2-io"            % "3.4.0",
      "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3" % Test
    )
  )
