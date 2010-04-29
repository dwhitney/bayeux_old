package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach, BeforeAndAfterAll}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.stm.HashTrie
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.remote._

class ChannelSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach with BeforeAndAfterAll{
	
	def clearChannels: Unit = {
	    ActorRegistry.actorsFor[Channel].foreach(_.stop)
	    ()
    }
	
	def all: List[Channel] = ActorRegistry.actorsFor[Channel]
	
	override def beforeEach: Unit = clearChannels
	
	override def beforeAll: Unit = ()
	override def afterAll: Unit = ()
	
	"A Channel" should "construct normally" in {
		val channel = Channel("/chat/scala")
		()
	}
	
	it should "throw an IllegalArgumentException if the channel name is null" in {
		evaluating{
			val channel = Channel(null)
			()
		} must produce [IllegalArgumentException]
	}

	
	it should "throw an IllegalArgumentsException when the beginning character isn't a /" in {
		evaluating { 
			val channel = Channel("chat/scala")
			() 
		} must produce [IllegalArgumentException]
	}
	
	it should "throw an IllegalArgumentsException when one of the characters isn't the type allowed in URL Encoding" in {
		evaluating { 
			val channel = Channel("/ch!at/scala")
			() 
		} must produce [IllegalArgumentException]
	}
	
	it should "not throw an IllegalArgumentException when + is in the segments" in {
		val channel = Channel("/ch+at/scala")
		()
	}
	
	it should "not throw an IllegalArgumentException when %20 is in the segments" in {
		val channel = Channel("/ch%20at/scala")
		()
	}
	
	it should "throw an IllegalArgumentException if the channel name has wildcards when the apply() method is called" in {
	    evaluating { 
			val channel = Channel("/**")
			() 
		} must produce [IllegalArgumentException]
	}
	
	it should "throw an IllegalArgumentException the last character is a slash" in {
	    evaluating { 
			val channel = Channel("/chat/")
			() 
		} must produce [IllegalArgumentException]
	}
	
	it should "return a list of one element when no wildcard is used with getChannels" in {
	    val channel = Channel("/chat/scala")
	    Channel.getChannels("/chat/scala") must equal (Set(channel))
	} 
	
	it should "return multiple matches on the /** pattern" in {
	    val channelOne = Channel("/chat/one")
	    val channelOneSubA = Channel("/chat/one/a")
	    Channel.getChannels("/chat/**") must equal (Set(channelOne, channelOneSubA))
	}
	
	it should "return multiple matches on the /** pattern (testing multiple channels that don't apply to the pattern)" in {
	    val channelOne = Channel("/chat/one")
	    val channelOneSubA = Channel("/chat/one/a")
	    val channelBlah = Channel("/blah/foo")
	    Channel.getChannels("/chat/**") must equal (Set(channelOne, channelOneSubA))
	}
	
	it should "return single level matches on the /* pattern " in {
	    val channelOne = Channel("/chat/one")
	    val channelOneSubA = Channel("/chat/one/a")
	    val channelBlah = Channel("/blah/foo")
	    Channel.getChannels("/chat/*") must equal (Set(channelOne))
	}
	
	it should "take subscriptions" in {
	    val client = Client.apply

	    val channel = Channel("/chat/scala")
	    channel.start
	    
	    channel ! Subscribe(client.uuid)
	    val subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
	    var testSubscribers = new HashTrie[String, Client]
	    testSubscribers = testSubscribers.update(client.uuid, client)

        subscribers must equal (testSubscribers)
	}
	
	it should "take unsubscriptions" in {
	    val client = Client.apply

	    val channel = Channel("/chat/scala")
	    channel.start
	    
	    channel ! Subscribe(client.uuid)
	    var subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
	    var testSubscribers = new HashTrie[String, Client]
	    testSubscribers = testSubscribers.update(client.uuid, client)
	    subscribers must equal (testSubscribers)
	    
	    channel ! Unsubscribe(client.uuid)
        
        subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
	    testSubscribers = new HashTrie[String, Client]
	    
        subscribers must equal (testSubscribers)
	}
	
	it must "publish messages to all of the clients in a channel when receiving a publish message" in {
	    import scala.collection.immutable.Queue
	    val channel = Channel("/chat/scala")
	    val client = Client.apply
	    channel ! Subscribe(client.uuid)
	    
	    val message = new Message(channel = "/chat/scala")
	    
	    channel ! Publish(message)
	    
	    val queue = Queue[Message]() enqueue message
	    //sleep to let actors all process messages
	    Thread.sleep(10)
	    (client !! GetMessageQueue).getOrElse(Queue[Message]()) must equal(queue)
	}
}