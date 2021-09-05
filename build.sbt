ThisBuild / scalaVersion := "3.0.1"

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf-8",  // Specify character encoding used by source files.
  "-deprecation",
  "-feature",
  "-explain",            // Explain errors in more detail.
  "-explain-types",      // Explain type errors in more detail.
  "-unchecked",          // Enable additional warnings where generated code depends on assumptions.
  "-Xfatal-warnings",    // Fail the compilation if there are any warnings.
  "-new-syntax",         // Require `then` and `do` in control expressions.
  "-rewrite",
  "-Ykind-projector:underscores",
//  "-Ykind-projector",
  "-source:future-migration",
)

ThisBuild / javacOptions := Seq(
  "-source", "11",
  "-Xlint:unchecked",
  "-Xlint:deprecation"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala3"
  )

lazy val exercises = (project in file("exercises"))
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(
    name := "answers"
  )
