crossScalaVersions := Seq("2.11.11", "2.12.2")

scalaVersion in ThisBuild := "2.12.2"

organization in ThisBuild := "typeformation"

val circeVersion = "0.8.0"

lazy val commonSettings = Seq(
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  scalacOptions ++= Seq(
    "-encoding", "UTF-8", // 2 args
    "-feature",
    "-deprecation",
    "-language:implicitConversions",
    "-unchecked",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard"
  ),
  organization := "typeformation",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-java8",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)
)

val macroSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise"))
)

lazy val cf = (project in file("cf")).
  settings(commonSettings: _*).
  settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.julienrf" %% "enum" % "3.1"
  ))


lazy val macros = (project in file("macros")).
  settings(commonSettings: _*).
  settings(macroSettings: _*).
  settings(
    scalacOptions ++= Seq(
      "-Yrangepos"
    ),
    libraryDependencies ++= Seq(
      "com.nrinaudo" %% "kantan.csv" % "0.1.19",
      "org.scalameta" %% "scalameta" % "1.8.0"
    ),
    (resourceGenerators in Compile) += Def.task {
      val targetFile = (resourceManaged in Compile).value / "cf-specs.json"

      if (!targetFile.exists) {
        val temp = System.getProperty("java.io.tmpdir")
        val tempFile = new File(s"$temp/cf-specs.json")
        println(s"Trying to get specs file from $tempFile")
        // Having a cache outside of the project is handy when using `sbt clean` often
        if (!tempFile.exists) {
          println("Getting specifications from AWS...")
          IO.download(new URL("https://d3teyb21fexa9r.cloudfront.net/latest/CloudFormationResourceSpecification.json"), tempFile)
        }
        IO.copyFile(tempFile, targetFile)
      }

      Seq(targetFile)
    }.taskValue
  ).
  dependsOn(cf)

lazy val resources = (project in file("resources")).
  settings(commonSettings: _*).
  settings(macroSettings: _*).
  dependsOn(macros)

lazy val root =
  (project in file(".")).
    aggregate(macros, resources, cf)
