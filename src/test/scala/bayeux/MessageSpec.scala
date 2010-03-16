package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.collection.HashTrie

//lift
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._




class MessageSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = Channel.clearChannels
	
	"A Message" should "construct normally" in {
		val channel = Channel("/chat/scala")
		val message = new Message(channel)
		()
	}
	
	it should "transform a /meta/connect response into a JValue" in {
	    import net.liftweb.json.JsonParser._
        val json = parse("""{"data": { "one" : 1, "two": "2", "three": {"four": true}, "five": [6,7,8] }}""")
        println(json)
        (json \ "data") match {
            case JField("data", obj: JObject) => println(obj.values.getClass)
            case _ => println("not found")
        }
        ()
        
	    ()
	}
	
}