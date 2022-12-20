ThisBuild / version := "0.2.0"

val logging = "commons-logging" % "commons-logging" % "1.2"
val fontbox = "org.apache.pdfbox" % "fontbox" % "2.0.18"
val pdfbox = "org.apache.pdfbox" % "pdfbox" % "2.0.18"
val funtest = "org.scalatest" %% "scalatest-funsuite" % "3.2.7"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "vqs",
      libraryDependencies += logging,
      libraryDependencies += fontbox,
      libraryDependencies += pdfbox,
      libraryDependencies += funtest
  )
