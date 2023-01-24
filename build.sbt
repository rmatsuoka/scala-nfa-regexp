
lazy val regexp = (project in file("."))
  .settings(
    name := "Regexp",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test,
  )
