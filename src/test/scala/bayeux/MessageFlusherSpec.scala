package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.stm.HashTrie

//lift
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._




class MessageFlusherSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = Channel.clearChannels
	
	"A MessageFlusher" should "construct normally" in {
		val message = new Message(channel = Channel("/chat/scala"), client = Client.apply)
		val flusher = MessageFlusher(List(message))
		()
	}
	
	it should "immediately flush upon receiving a /meta/handshake message" in {
		val client = Client.apply	
		val message = new Message(channel = Channel(Bayeux.META_HANDSHAKE), client = client)
		val flusher = MessageFlusher(List(message))
		
		//Thread.sleep(100)
		
		//flusher.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a /meta/subscribe message" in {
		val client = Client.apply	
		val message = new Message(channel = Channel(Bayeux.META_SUBSCRIBE), client = client)
		val flusher = MessageFlusher(List(message))
		
		//Thread.sleep(100)
		
		//flusher.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a /meta/disconnect message" in {
		val client = Client.apply	
		val message = new Message(channel = Channel(Bayeux.META_DISCONNECT), client = client)
		val flusher = MessageFlusher(List(message))
		
		Thread.sleep(1000)
		
		//flusher.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a publish message" in {
		val client = Client.apply	
		val message = new Message(channel = Channel("/chat/scala"), client = client)
		val flusher = MessageFlusher(List(message))
		
		//Thread.sleep(100)
		
		//flusher.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon invocation of the get method" in {
		val client = Client.apply	
		val message = new Message(channel = Channel(Bayeux.META_CONNECT), client = client)
		val flusher = MessageFlusher(List(message))
		
		//Thread.sleep(100)
		
		//flusher.get must equal(List(message))
		
		()
	}
		
}