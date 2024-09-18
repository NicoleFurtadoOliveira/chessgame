val scala3Version = "3.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "chessgame",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    //libraryDependencies += 

    unmanagedJars in Compile += baseDirectory.value / "lib" / "userinput.jar"
  )
