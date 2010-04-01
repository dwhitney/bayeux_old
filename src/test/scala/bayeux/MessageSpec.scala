package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.stm.HashTrie
import se.scalablesolutions.akka.actor._

//lift
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

class MessageSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = ActorRegistry.actorsFor[Channel].foreach(_.stop)
	
	"A Message" should "construct normally" in {
		val channel = Channel("/chat/scala")
		val message = new Message(channel = channel)
		()
	}
	
	it should "transform a map into a JObject" in {
	    import us.says.bayeux.Message._
        val json: JObject = mapToJObject(Map("list" -> List(0,1,2,3), "map" -> Map("foo" -> "bar", "anotherList" -> List(true, false, 3, "four")), "one" -> "1", "two" -> "two", "three" -> 3, "four" -> true, "five" -> null, "six" -> 6.5, "date" -> new java.util.Date().getClass))
        compact(JsonAST.render(json)) must equal("""{"list":[0,1,2,3],"one":"1","two":"two","five":null,"map":{"foo":"bar","anotherList":[true,false,3,"four"]},"four":true,"three":3,"date":"class java.util.Date","six":6.5}""")
	}
	
	it should "transform in to a Message" in {
	    import us.says.bayeux.Message._
	    val json: JObject = mapToJObject(Map("list" -> List(0,1,2,3), "map" -> Map("foo" -> "bar", "anotherList" -> List(true, false, 3, "four")), "one" -> "1", "two" -> "two", "three" -> 3, "four" -> true, "five" -> null, "six" -> 6.5, "date" -> new java.util.Date().getClass))
	    val message = new Message(json)
	    ()
	}
	
	it should "transform from a message into JSON" in {
	    import us.says.bayeux.Message._

		val json2 = Message(channel = Channel(Bayeux.META_HANDSHAKE))
		println(json2)
	    ()
	}
	
}