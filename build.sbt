lazy val root = (project in file(".")).
  settings(
    name := "ChessGamesAnalyzer",
    version := "1.0",
    scalaVersion := "2.10.4",
    mainClass in Compile := Some("a8.Main")
  )

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

// META-INF discarding
mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
   {
    case PathList("reference.conf") => MergeStrategy.concat
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case x => MergeStrategy.first
   }
}





