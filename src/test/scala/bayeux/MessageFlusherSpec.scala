package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.collection.HashTrie

//lift
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._




class MessageFlusherSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = Channel.clearChannels
	
	"A MessageFlusher" should "construct normally" in {
		val message = new Message(channel = Channel("/chat/scala"), client = new Client)
		val flusher = new MessageFlusher(message)
		()
	}
	
	it should "immediately flush upon receiving a /meta/handshake message" in {
		val client = new Client()		
		val message = new Message(channel = Channel(Bayeux.META_HANDSHAKE), client = client)
		val flusher = new MessageFlusher(message)
		
		Thread.sleep(100)
		
		flusher.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a /meta/subscribe message" in {
		val client = new Client()		
		val message = new Message(channel = Channel(Bayeux.META_SUBSCRIBE), client = client)
		val flusher = new MessageFlusher(message)
		
		Thread.sleep(100)
		
		flusher.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a /meta/disconnect message" in {
		val client = new Client()		
		val message = new Message(channel = Channel(Bayeux.META_DISCONNECT), client = client)
		val flusher = new MessageFlusher(message)
		
		Thread.sleep(100)
		
		flusher.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a publish message" in {
		val client = new Client()		
		val message = new Message(channel = Channel("/chat/scala"), client = client)
		val flusher = new MessageFlusher(message)
		
		Thread.sleep(100)
		
		flusher.isDone must be(true)
		
		()
	}
	
}