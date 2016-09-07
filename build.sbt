name := "algorithms_of_the_intelligent_web"

organization := "com.github.interaction"

version := "0.1.0"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.6", "2.11.8")

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Artima Maven Repository" at "http://repo.artima.com/releases" 
)


libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
// http://www.scala-lang.org/files/archive/nightly/docs-2.10.2/manual/html/scalac.html
// http://blog.threatstack.com/useful-scalac-options-for-better-scala-development-part-1
scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",// Enable recommended additional warnings.
    "-Yinline-warnings",
    "-Ywarn-dead-code",
    "-Xfuture",
    "-Yno-adapted-args"
)

initialCommands := "import iweb.ch04.similarity._"