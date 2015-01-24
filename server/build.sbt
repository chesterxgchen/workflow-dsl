import com.typesafe.sbt.SbtStartScript

resolvers += Resolver.url("scala-js-releases",
  url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
    Resolver.ivyStylePatterns)

resolvers += Resolver.url("scala-js-snapshots",
  url("http://repo.scala-js.org/repo/snapshots/"))(
    Resolver.ivyStylePatterns)


libraryDependencies ++= Seq(
  "com.lihaoyi" % "utest_2.10" % "0.1.2" % "test",
  "org.scala-lang" % "scala-compiler" % "2.10.3",
  "org.scala-lang" % "scala-reflect" % "2.10.3",
  "com.typesafe.akka" %% "akka-actor" % "2.3.0",
  //"com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "io.spray" % "spray-can" % "1.3.1",
  "io.spray" % "spray-client" % "1.3.1",
  "io.spray" % "spray-caching" % "1.3.1",
  "io.spray" % "spray-httpx" % "1.3.1",
  "io.spray" % "spray-routing" % "1.3.1",
  "io.spray" %% "spray-json" % "1.2.5",
  "org.scala-lang.modules.scalajs" %% "scalajs-compiler" % "0.4.2",
  "com.typesafe.play" %% "play-json" % "2.2.2",
  "org.scala-lang.modules.scalajs" %% "scalajs-tools" % "0.4.2",
  "org.scala-lang.modules" %% "scala-async" % "0.9.0" % "provided",
  "com.scalatags" % "scalatags_2.10" % "0.2.4",
  "com.lihaoyi" %% "acyclic" % "0.1.1" % "provided",
  "org.webjars" % "ace" % "07.31.2013",
  "org.webjars" % "jquery" % "2.1.0-2",
  "org.webjars" % "normalize.css" % "2.1.3",
  "org.apache.spark" % "spark-core_2.10" % "1.0.0",
  "org.apache.spark" % "spark-sql_2.10" % "1.0.0",
  "org.apache.spark" % "catalyst_2.10" % "1.0.0",
 "mysql" % "mysql-connector-java" % "5.1.30"
)

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

testFrameworks += new TestFramework("utest.runner.JvmFramework")

SbtStartScript.startScriptForClassesSettings

javaOptions += "-XX:MaxPermSize=2g"

Revolver.settings

javaOptions in Revolver.reStart += "-Xmx2g"

addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.1")

autoCompilerPlugins := true
