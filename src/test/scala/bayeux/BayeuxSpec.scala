package us.says.bayeux

//scala
import scala.collection.immutable.Queue

//scalatest
import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.collection.HashTrie

class BayeuxSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = Channel.clearChannels
	
	"A Bayeux" should "include the handshake channel when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.channel must equal(Channel(Bayeux.META_HANDSHAKE))
		()
	}
	
	it must "include a version when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.version must equal (Bayeux.VERSION)
	}
	
	it must "include supported connection types when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.supportedConnectionTypes must equal (Bayeux.SUPPORTED_CONNECTION_TYPES)
	}
	
	it must "include a newly created client when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        println(response.error)
        response.client must not be (null)
	}

    it must "include successful=true responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.successful must be (true)
	}
	
	it must "include the same id as the one included in the handshake responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        message.id = "myId"
        val response = TestBayeux.dispatch(message).get
        response.id must equal (message.id)
	}
	
	it must "create an error message when a version is not included in a /meta/handshake from client" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        message.version = null
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal(String.format("%s:%s:the version specified is incompatible with this implementation of Bayeux", Bayeux.ERROR_INVALID_VERSION.toString, null))
	}
	
	it must "send an error message when a client is not included in a /meta/connect message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_CONNECT))
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send an error message when a connectionType is not included in a /meta/connect mesage" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_CONNECT), new Client)
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("404:null:a connectionType was not specified")
	}
	
	it must "send an error message when a connectionType is not supported in a /meta/connect mesage" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_CONNECT), new Client)
        message.connectionType = "web-socket"
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("405:web-socket:the connectionType specified is unsupported")
	}
	
	it must """
	    contain the /meta/connect channel, 
	    successful must be true 
	    the client must not be null
	    the id of the client in the response must match the id of the client in the request
	    the id of the response message must match the id of the request message
	    in the response message to the client
	    """ in {
	    object TestBayeux extends Bayeux{}
	    val client = new Client
        val message = new Message(Channel(Bayeux.META_CONNECT), client)
        message.connectionType = "long-polling"
        message.id = "myId"
        val response = TestBayeux.dispatch(message).get
        response.error must be(null)
        response.successful must be(true)
        response.client must not be(null)
        response.client.uuid must equal(client.uuid)
        response.id must equal(message.id)
        response.timestamp must not be(null)
	}
	
	it must "send an error message when a client is not included in a /meta/disconnect message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_DISCONNECT))
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send a response with the /meta/disconnect channel, the clientId, and successful = true and the client must be stopped" in {
	    object TestBayeux extends Bayeux{}
	    val client = new Client
	    Channel("/chat/scala") ! Subscribe(client)
	    (Channel("/chat/scala") !! GetSubscribers).getOrElse(HashTrie[String, Client]()).size must equal(1)
        val message = new Message(Channel(Bayeux.META_DISCONNECT), client)
        message.id = "myId"
        val response = TestBayeux.dispatch(message).get
        response.client must not be(null)
        response.successful must be(true)
        //sleep to let the actors process messages or else the test will fail
        Thread.sleep(50)
        (Channel("/chat/scala") !! GetSubscribers).getOrElse(HashTrie[String, Client]()).size must equal(0)
        response.id must equal(message.id)
        response.client.isRunning must be(false)
	}
	
	it must "send an error message when a client is not included in a /meta/subscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_SUBSCRIBE))
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send an error message when a subscription channel is not included in a /meta/subscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_SUBSCRIBE), new Client)
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("406:null:no subscription was specified")
	}
	
	it must """
	    set the channel to /meta/subscribe
	    set the clientId
	    set successful = true
	    set the id of the response to the id of the request
	    subscribe the client to the channel
	""" in {
	    object TestBayeux extends Bayeux{}
	    val client = new Client
        val message = new Message(Channel(Bayeux.META_SUBSCRIBE), client)
        message.id = "myId"
        message.subscription = Channel("/chat/scala")
        val response = TestBayeux.dispatch(message).get
        response.client must equal(client)
        response.subscription must equal(message.subscription)
        response.channel must equal (Channel(Bayeux.META_SUBSCRIBE))
        response.id must equal (message.id)
        val channel = Channel("/chat/scala")
        val subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
        subscribers.size must equal(1)
        subscribers(client.uuid) must equal(client)
	}
	
	it must "send an error message when a client is not included in a /meta/unsubscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_UNSUBSCRIBE))
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send an error message when a subscription channel is not included in a /meta/unsubscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_UNSUBSCRIBE), new Client)
        val response = TestBayeux.dispatch(message).get
        response.successful must be(false)
        response.error must equal("406:null:no subscription was specified")
	}
	
	it must """
	    set the channel to /meta/unsubscribe
	    set the clientId
	    set successful = true
	    set the id of the response to the id of the request
	    unsubscribe the client to the channel
	""" in {
	    object TestBayeux extends Bayeux{}
	    val client = new Client
	    val channel = Channel("/chat/scala")
	    
	    channel ! Subscribe(client)
	    var subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
        subscribers.size must equal(1)
        
        val message = new Message(Channel(Bayeux.META_UNSUBSCRIBE), client)
        message.id = "myId"
        message.subscription = Channel("/chat/scala")
        
        val response = TestBayeux.dispatch(message).get
        response.client must equal(client)
        response.subscription must equal(message.subscription)
        response.channel must equal (Channel(Bayeux.META_UNSUBSCRIBE))
        response.id must equal (message.id)
        
        subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
        subscribers.size must equal(0)
	}
	
	it must "return an error when no channel is specified on a publish message" in {
	    object TestBayeux extends Bayeux{}
	    
	    val message = new Message(null, null)
	    val response = TestBayeux.dispatch(message).get
	    response.error must equal("407:null:no channel was specified")
	}
	
	it must "publish messages to all of the clients in a channel when receiving a publish message" in {
	    object TestBayeux extends Bayeux{}
	    
	    val channel = Channel("/chat/scala")
	    val client = new Client
	    channel ! Subscribe(client)
	    
	    val message = new Message(Channel("/chat/scala"))
	    TestBayeux.dispatch(message)
	    
	    val queue = Queue[Message]() enqueue message
	    //sleep to let actors all process messages
	    Thread.sleep(50)
	    (client !! GetMessageQueue).getOrElse(Queue[Message]()) must equal(queue)
	}
}