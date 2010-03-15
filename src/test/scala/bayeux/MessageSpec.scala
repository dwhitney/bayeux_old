package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.collection.HashTrie

//lift
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonDSL._


class MessageSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = Channel.clearChannels
	
	"A Message" should "construct normally" in {
		val channel = Channel("/chat/scala")
		val message = new Message(channel)
		()
	}
	
	it should "transform a /meta/connect response into a JValue" in {
	//    val response = new Message(Channel(Bayeux.META_HANDSHAKE), new Client)
	 //   val json = (Message.CHANNEL -> Bayeux.META_HANDSHAKE)
	 //   new Message(json)
	    ()
	}
	
}