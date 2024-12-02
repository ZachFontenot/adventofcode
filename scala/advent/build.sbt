import org.typelevel.sbt.tpolecat.*

ThisBuild / organization := "com.zf"
ThisBuild / scalaVersion := "3.4.0"

// This disables fatal-warnings for local development. To enable it in CI set the `SBT_TPOLECAT_CI` environment variable in your pipeline.
// See https://github.com/typelevel/sbt-tpolecat/?tab=readme-ov-file#modes
ThisBuild / tpolecatDefaultOptionsMode := VerboseMode

lazy val root = (project in file(".")).settings(
  name := "advent",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.5.3",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.5.3",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.5.3",
    "org.typelevel" %% "cats-parse" % "1.0.0",
    "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test
  )
)
