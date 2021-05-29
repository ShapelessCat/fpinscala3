val commonSettings = Seq(
  scalaVersion := "3.0.0",

  javacOptions := Seq("-source", "11", "-Xlint:unchecked", "-Xlint:deprecation"),

  scalacOptions := Seq(
    "-encoding", "utf-8",  // Specify character encoding used by source files.
    "-deprecation",        // Emit warning and location for usages of deprecated APIs.
    "-feature",            // Emit warning and location for usages of features that should be imported explicitly.
    "-explain",            // Explain errors in more detail.
    "-explain-types",      // Explain type errors in more detail.
    "-unchecked",          // Enable additional warnings where generated code depends on assumptions.
    "-Xfatal-warnings",    // Fail the compilation if there are any warnings.
    "-new-syntax",         // Require `then` and `do` in control expressions.
    "-rewrite",
    "-source:future-migration",
  )
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
