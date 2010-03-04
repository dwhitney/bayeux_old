import sbt._

class SaysUsProject(info: ProjectInfo) extends DefaultWebProject(info){
	
	val liftJson = "net.liftweb" % "lift-json" % "1.1-M7"
	val ehcache = "net.sf.ehcache" % "ehcache" % "1.7.0"
	val slf4j = "org.slf4j" % "slf4j-simple" % "1.3.1"
	val junit = "junit" % "junit" % "4.7"
	val mockito = "org.mockito" % "mockito-core" % "1.8.2"
	val scalaTest = "org.scalatest" % "scalatest" % "1.0"
	
	
	val akka = "se.scalablesolutions.akka" % "akka-core" % "0.6"
    val akka_redis = "se.scalablesolutions.akka" % "akka-persistence-redis" % "0.6"
    val akka_util = "se.scalablesolutions.akka" % "akka-util" % "0.6"
	
	val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
	val mavenRepo2 = "Maven Repository 2" at "http://repo2.maven.org/maven2/"
    val multiverse = "Multiverse" at "http://multiverse.googlecode.com/svn/maven-repository/snapshots"
    val multiverseReleases = "Multiverse Releases" at "http://multiverse.googlecode.com/svn/maven-repository/releases/"
    val akka_repo = "Akka Maven Repository" at "http://scalablesolutions.se/akka/repository"
	
}