package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.collection.HashTrie

class ClientSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = {
	    Channel.clearChannels
	    Client.clearClients
	}
	
	"A Client" should "construct normally and add itself to the clients list" in {
		val client = new Client
		val clients = Client.getClients
		client must equal(clients.get(client.uuid).get)
		()
	}
	
	it should "add a channel to its subscriptions when receiving a Subscribe message" in {
	    val client = new Client
	    client.start
	    val channel = Channel("/chat/scala")
	    client ! AddSubscription(channel)
	    val subscriptions = (client !! GetSubscriptions).getOrElse(Set[Channel]())
	    subscriptions must equal(Set(channel))
	    
	    val subscribers = (channel !! GetSubscribers).getOrElse(HashTrie[String, Client]())
	    subscribers.size must equal (1)
	    subscribers(client.uuid) must equal(client)
	}
	
	it should "remove a channel from its subscriptions when receiving a Subscribe message" in {
	    val client = new Client
	    client.start
	    val channel = Channel("/chat/scala")
	    client ! AddSubscription(channel)
	    var subscriptions = (client !! GetSubscriptions).getOrElse(Set[Channel]())
	    subscriptions must equal(Set(channel))
	    
	    var subscribers = (channel !! GetSubscribers).getOrElse(HashTrie[String, Client]())
	    subscribers.size must equal (1)
	    subscribers(client.uuid) must equal(client)
	    
	    client ! RemoveSubscription(channel)
	    subscriptions = (client !! GetSubscriptions).getOrElse(Set[Channel](Channel("/chat/scala")))
	    subscriptions must equal(Set[Channel]())
	    
	    subscribers = (channel !! GetSubscribers).getOrElse(HashTrie[String, Client]() + (client.uuid -> client))
	    subscribers.size must equal (0)
	}
	
	it should "unsubscribe to all channels and stop when receiving a Disconnect" in {
	    val client = new Client
	    client.start
	    
	    val scala = Channel("/chat/scala")
	    val play = Channel("/chat/play")
	    
	    client ! AddSubscription(scala)
	    client ! AddSubscription(play)
	    
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
	    val client = new Client	    
	    val message = new Message(Channel("/chat/scala"))
	    
	    client ! Enqueue(message)
	    
	    val queue = Queue[Message]() enqueue message
	    (client !! GetMessageQueue).getOrElse(Queue[Message]()) must equal(queue)
	}
	
}