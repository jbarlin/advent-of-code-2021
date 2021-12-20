val scala3Version = "3.1.0"

lazy val root = project
    .in(file("."))
    .settings(
      name         := "aoc",
      version      := "0.1.0-SNAPSHOT",
      scalaVersion := scala3Version,
      // https://mvnrepository.com/artifact/org.apache.commons/commons-lang3
      libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.12.0",
      libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4", 
      scalacOptions ++= Seq(
        "-optimisations",
        "-optimise",
        "-opt:l:method",
      )
    )
