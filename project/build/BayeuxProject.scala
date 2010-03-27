import sbt._

class BayeuxProject(info: ProjectInfo) extends DefaultProject(info){
	
	//val liftJson = "net.liftweb" % "lift-json" % "1.1-M7"
	val mockito = "org.mockito" % "mockito-core" % "1.8.2" % "test"
	val scalaTest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" % "test"
	//val liftJson = "net.liftweb" % "lift-json" % "2.0-M3" % "compile"
	val jodaTime = "joda-time" % "joda-time" % "1.6" % "compile"
	val configgy = "net.lag" % "configgy" % "2.8.0.Beta1-1.5-SNAPSHOT" % "compile"
	val werkz = "org.codehaus.aspectwerkz" % "aspectwerkz-nodeps-jdk5" % "2.1" % "compile"
    val werkz_core = "org.codehaus.aspectwerkz" % "aspectwerkz-jdk5" % "2.1" % "compile"
    val multiverse = "org.multiverse" % "multiverse-alpha" % "0.4" % "compile"
    val guicey = "org.guiceyfruit" % "guice-core" % "2.0-beta-4" % "compile"
    val protobuf = "com.google.protobuf" % "protobuf-java" % "2.2.0" % "compile"
	
	val log4j = "log4j" % "log4j" % "1.2.9" % "compile"
	//val akka = "se.scalablesolutions.akka" % "akka-core" % "0.7"
	
	val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
	val mavenRepo2 = "Maven Repository 2" at "http://repo2.maven.org/maven2/"
    val multiverseRepo = "Multiverse" at "http://multiverse.googlecode.com/svn/maven-repository/snapshots"
    val multiverseReleases = "Multiverse Releases" at "http://multiverse.googlecode.com/svn/maven-repository/releases/"
    //val akka_repo = "Akka Maven Repository" at "http://scalablesolutions.se/akka/repository"
    val lagRepo = "Lag repo for configgy" at "http://www.lag.net/repo/"
    val toolsSnapShots = "Scala Tools Snapshots Repo" at "http://www.scala-tools.org/repo-snapshots"
	
}