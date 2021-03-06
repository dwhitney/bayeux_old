package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.stm.HashTrie
import se.scalablesolutions.akka.actor._

class ClientSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = {
	    ActorRegistry.actorsFor[Channel].foreach(_.stop)
	    ActorRegistry.actorsFor[Client].foreach(_.stop)
	}
	
	"A Client" should "construct normally and add itself to the clients list" in {
		val client = Client.apply
		val clients = ActorRegistry.actorsFor[Client]
		client must equal(clients.find(_.uuid == client.uuid).get)
		()
	}
	
	it should "add a channel to its subscriptions when receiving a Subscribe message" in {
	    val client = Client.apply
	    client.start
	    val channel = Channel("/chat/scala")
	    client ! AddSubscription(channel.name)
	    val subscriptions = (client !! GetSubscriptions).getOrElse(Set[Channel]())
	    subscriptions must equal(Set(channel))
	    
	    val subscribers = (channel !! GetSubscribers).getOrElse(HashTrie[String, Client]())
	    subscribers.size must equal (1)
	    subscribers(client.uuid) must equal(client)
	}
	
	it should "remove a channel from its subscriptions when receiving a Subscribe message" in {
	    val client = Client.apply
	    client.start
	    val channel = Channel("/chat/scala")
	    client ! AddSubscription(channel.name)
	    var subscriptions = (client !! GetSubscriptions).getOrElse(Set[Channel]())
	    subscriptions must equal(Set(channel))
	    
	    var subscribers = (channel !! GetSubscribers).getOrElse(HashTrie[String, Client]())
	    subscribers.size must equal (1)
	    subscribers(client.uuid) must equal(client)
	    
	    client ! RemoveSubscription(channel.name)
	    subscriptions = (client !! GetSubscriptions).getOrElse(Set[Channel](Channel("/chat/scala")))
	    subscriptions must equal(Set[Channel]())
	    
	    subscribers = (channel !! GetSubscribers).getOrElse(HashTrie[String, Client]() + (client.uuid -> client))
	    subscribers.size must equal (0)
	}
	
	it should "unsubscribe to all channels and stop when receiving a Disconnect" in {
	    val client = Client.apply
	    client.start
	    
	    val scala = Channel("/chat/scala")
	    val play = Channel("/chat/play")
	    
	    client ! AddSubscription(scala.name)
	    client ! AddSubscription(play.name)
	    
	    (client !! GetSubscriptions).getOrElse(Set[Channel]()) must equal(Set[Channel]() + scala + play)
	    
	    
	    client ! Disconnect
	    //sleep to give actors time to process messages
	    Thread.sleep(10)
	    client.isRunning must be (false)
	    (scala !! GetSubscribers).getOrElse(HashTrie[String, Client]() + (client.uuid -> client)).size must equal(0)
	    (play !! GetSubscribers).getOrElse(HashTrie[String, Client]() + (client.uuid -> client)).size must equal(0)
	}
	
	it must "enqueue a message in its message queue when receiving an enqueue message" in {
	    import scala.collection.immutable.Queue
	    val client = Client.apply	    
	    val message = new Message(channel = "/chat/scala")
	    
	    client ! Enqueue(message)
	    
	    val queue = Queue[Message]() enqueue message
	    (client !! GetMessageQueue).getOrElse(Queue[Message]()) must equal(queue)
	}
	
	it must "flush a list of messages when the Flush message is sent" in {
	    import scala.collection.immutable.Queue
	    val client = Client.apply	    
	    val message = new Message(channel = "/chat/scala")
	    client.isDone must be (false)
	    client ! Enqueue(message)
	    Thread.sleep(100)
	    client.isDone must be (true)
	    
	    List(message) must equal ((client !! Flush).getOrElse(List[Message]()))
	}
	
	it should "immediately flush upon receiving a /meta/handshake message" in {
		val client = Client.apply	
		val message = new Message(channel = Bayeux.META_HANDSHAKE, clientId = client.uuid)
		
		client ! Enqueue(message)
		
        Thread.sleep(100)
		
        client.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a /meta/subscribe message" in {
		val client = Client.apply	
		val message = new Message(channel = Bayeux.META_SUBSCRIBE, clientId = client.uuid)
		
		client ! Enqueue(message)
		
        Thread.sleep(100)
		
        client.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a /meta/disconnect message" in {
		val client = Client.apply	
		val message = new Message(channel = Bayeux.META_DISCONNECT, clientId = client.uuid)
		
		client ! Enqueue(message)
		
        Thread.sleep(100)
		
        client.isDone must be(true)
		
		()
	}
	
	it should "immediately flush upon receiving a publish message" in {
		val client = Client.apply	
		val message = new Message(channel = "/chat/scala", clientId = client.uuid)
	    client ! Enqueue(message)
		
        Thread.sleep(100)
		
        client.isDone must be(true)
		()
	}
	
	it should "immediately flush upon invocation of the get method" in {
		val client = Client.apply	
		val message = new Message(channel = Bayeux.META_CONNECT, clientId = client.uuid)
		client ! Enqueue(message)
		
        Thread.sleep(100)
		
		client.get must equal(List(message))
		
		()
	}
}