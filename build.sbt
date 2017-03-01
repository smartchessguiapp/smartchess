

import com.github.retronym.SbtOneJar._

oneJarSettings

libraryDependencies += "commons-io" % "commons-io" % "2.5"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.4"

scalacOptions ++= Seq("-feature")

val ps = new sys.SystemProperties
val jh = ps("java.home")
val jdkh = jh.replaceAll("jre","jdk")
val f_jdkh = file(jdkh)
val f_libextjavafx = file(jh) / "lib/ext/jfxrt.jar"

javaHome := Some(f_jdkh)

unmanagedJars in Compile +=
{
	println("javaHome: "+f_jdkh+"\nunmanagedJars+: "+f_libextjavafx)
	Attributed.blank(f_libextjavafx)
}

addCommandAlias("c","~compile")

name := "smartchess"

version := "1.0"

scalaVersion := "2.11.8"

