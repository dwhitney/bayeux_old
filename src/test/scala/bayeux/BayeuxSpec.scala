package us.says.bayeux

//scala
import scala.collection.immutable.Queue

//scalatest
import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.stm.HashTrie
import se.scalablesolutions.akka.actor._

class BayeuxSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = ActorRegistry.actorsFor[Channel].foreach(_.stop)
	
	"A Bayeux" should "include the handshake channel when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_HANDSHAKE)
        val response = TestBayeux.dispatch(message).get(0)
        response.channel must equal(Bayeux.META_HANDSHAKE)
		()
	}
	
	it must "include a version when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_HANDSHAKE)
        val response = TestBayeux.dispatch(message).get(0)
        response.version must equal (Bayeux.VERSION)
	}
	
	it must "include supported connection types when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_HANDSHAKE)
        val response = TestBayeux.dispatch(message).get(0)
        response.supportedConnectionTypes must equal (Bayeux.SUPPORTED_CONNECTION_TYPES)
	}
	
	it must "include a newly created client when responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_HANDSHAKE)
        val response = TestBayeux.dispatch(message).get(0)
        println(response.error)
        response.clientId must not be (null)
	}

    it must "include successful=true responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_HANDSHAKE)
        val response = TestBayeux.dispatch(message).get(0)
        response.successful must be (true)
	}
	
	it must "include the same id as the one included in the handshake responding to a /meta/handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(
            channel = Bayeux.META_HANDSHAKE,
            id = "myId")

        val response = TestBayeux.dispatch(message).get(0)
        response.id must equal (message.id)
	}
	
	it must "create an error message when a version is not included in a /meta/handshake from client" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_HANDSHAKE,
            version = null)

        val response = TestBayeux.dispatch(message).get(0)
        response.successful must be(false)
        response.error must equal(String.format("%s:%s:the version specified is incompatible with this implementation of Bayeux", Bayeux.ERROR_INVALID_VERSION.toString, null))
	}
	
	it must "send an error message when a client is not included in a /meta/connect message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_CONNECT)
        val response = TestBayeux.dispatch(message).get(0)
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send an error message when a connectionType is not included in a /meta/connect mesage" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_CONNECT, clientId = Client.apply.uuid)
        val response = TestBayeux.dispatch(message).get(0)
        response.successful must be(false)
        response.error must equal("404:null:a connectionType was not specified")
	}
	
	it must "send an error message when a connectionType is not supported in a /meta/connect mesage" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_CONNECT, clientId = Client.apply.uuid, connectionType = "web-socket")
        val response = TestBayeux.dispatch(message).get(0)
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
	    val client = Client.apply
        val message = new Message(channel = Bayeux.META_CONNECT, clientId = client.uuid,
            connectionType = "long-polling",
            id = "myId")
        
        TestBayeux.dispatch(message) must equal (None)
        
        val response = (client !! GetMessageQueue).getOrElse(Queue[Message]()).front
        response.error must be(null)
        response.successful must be(true)
        response.clientId must not be(null)
        response.clientId must equal(client.uuid)
        response.id must equal(message.id)
        response.timestamp must not be(null)
	}
	
	it must "send an error message when a client is not included in a /meta/disconnect message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_DISCONNECT)
        val response = TestBayeux.dispatch(message).get(0)
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send a response with the /meta/disconnect channel, the clientId, and successful = true and the client must be stopped" in {
	    object TestBayeux extends Bayeux{}
	    val client = Client.apply
	    Channel("/chat/scala") ! Subscribe(client.uuid)
	    (Channel("/chat/scala") !! GetSubscribers).getOrElse(HashTrie[String, Client]()).size must equal(1)
        val message = new Message(channel = Bayeux.META_DISCONNECT, clientId = client.uuid, id = "myId")

        val response = TestBayeux.dispatch(message).get(0)
        response.clientId must not be(null)
        response.successful must be(true)
        //sleep to let the actors process messages or else the test will fail
        Thread.sleep(50)
        (Channel("/chat/scala") !! GetSubscribers).getOrElse(HashTrie[String, Client]()).size must equal(0)
        response.id must equal(message.id)
	}
	
	it must "send an error message when a client is not included in a /meta/subscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel =  Bayeux.META_SUBSCRIBE)
        val response = TestBayeux.dispatch(message).get(0)
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send an error message when a subscription channel is not included in a /meta/subscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_SUBSCRIBE, clientId = Client.apply.uuid)
        val response = TestBayeux.dispatch(message).get(0)
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
	    val client = Client.apply
        val message = new Message(channel = Bayeux.META_SUBSCRIBE, clientId = client.uuid, id = "myId", subscription = "/chat/scala")
        val response = TestBayeux.dispatch(message).get(0)
        response.clientId must equal(client.uuid)
        response.subscription must equal(message.subscription)
        response.channel must equal (Bayeux.META_SUBSCRIBE)
        response.id must equal (message.id)
        val channel = Channel("/chat/scala")
        val subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
        subscribers.size must equal(1)
        subscribers(client.uuid) must equal(client)
	}
	
	it must "send an error message when a client is not included in a /meta/unsubscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_UNSUBSCRIBE)
        val response = TestBayeux.dispatch(message).get(0)
        response.successful must be(false)
        response.error must equal("403:null:either a clientId was not sent, or it was not found")
	}
	
	it must "send an error message when a subscription channel is not included in a /meta/unsubscribe message" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(channel = Bayeux.META_UNSUBSCRIBE, clientId = Client.apply.uuid)
        val response = TestBayeux.dispatch(message).get(0)
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
	    val client = Client.apply
	    val channel = Channel("/chat/scala")
	    
	    channel ! Subscribe(client.uuid)
	    var subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
        subscribers.size must equal(1)
        
        val message = new Message(channel = Bayeux.META_UNSUBSCRIBE, clientId = client.uuid, id = "myId", subscription = "/chat/scala")
        
        val response = TestBayeux.dispatch(message).get(0)
        response.clientId must equal(client.uuid)
        response.subscription must equal(message.subscription)
        response.channel must equal (Bayeux.META_UNSUBSCRIBE)
        response.id must equal (message.id)
        
        subscribers = (channel !! GetSubscribers).getOrElse(new HashTrie[String, Client])
        subscribers.size must equal(0)
	}
	
	it must "return an error when no channel is specified on a publish message" in {
	    object TestBayeux extends Bayeux{}
	    
	    val message = new Message(null, null)
	    val response = TestBayeux.dispatch(message).get(0)
	    response.error must equal("407:null:no channel was specified")
	}
	
	it must """
	            publish messages to all of the clients in a channel when receiving a publish message, 
	            but the client that sent the message should not receive the message via the channel, but instead,
	            have it sent as a response along with the aknowledgement""" in {
	    object TestBayeux extends Bayeux{}
	    
	    val channel = Channel("/chat/scala")
	    val client = Client.apply
	    channel ! Subscribe(client.uuid)
	    
	    val message = new Message(channel = "/chat/scala", clientId = client.uuid)
	    val publishList = TestBayeux.dispatch(message).get
	    publishList.size must equal(2) //it responds with an aknowledgement and the message response, so that more that an open connection doesn't need to be terminated
	    
	    val queue = Queue[Message]()
	    //sleep to let actors all process messages
	    Thread.sleep(50)
	    (client !! GetMessageQueue).getOrElse(Queue[Message](message)) must equal(queue)
	}
	
	it must "register an extension and call its registered method" in {
	    object TestBayeux extends Bayeux{}
	    var iWasCalled = false
	    object TestExtension extends Extension{
	        override def registered = {iWasCalled = true; ()}
	    }
	    
	    TestBayeux.registerExtension(TestExtension)
	    
	    iWasCalled must be(true)
	}
	
	it must "unregister an extension and call its unregistered method" in {
	    object TestBayeux extends Bayeux{}
	    var iWasCalled = false
	    object TestExtension extends Extension{
	        override def unregistered = {iWasCalled = true; ()}
	    }
	    
	    TestBayeux.registerExtension(TestExtension)
	    iWasCalled must be(false)
	    
	    TestBayeux.unregisterExtension(TestExtension)
	    iWasCalled must be(true)
	}
	
	it must "return an error message when a subscription to a /meta/ channel is attempted" in {
	    object TestBayeux extends Bayeux{}
	    val client = Client.apply
	    val message = new Message(channel = Bayeux.META_SUBSCRIBE, clientId = client.uuid, id = "myId", subscription = Bayeux.META_SUBSCRIBE)
	    
	    val response = TestBayeux.dispatch(message).get(0)
	    response.error must equal("409:null:you attempted to subscribe to a meta channel")
	}
	
	it must "return an error message when a subscription to a /service/ channel is attempted" in {
	    object TestBayeux extends Bayeux{}
	    val client = Client.apply
	    val message = new Message(channel = Bayeux.META_SUBSCRIBE, clientId = client.uuid, id = "myId", subscription = "/service/private")
	    
	    val response = TestBayeux.dispatch(message).get(0)
	    response.error must equal("410:null:you attempted to subscribe to a service channel")
	}
	
	it must "apply the incoming method of the extension when the message is called" in {
	    object TestBayeux extends Bayeux{}
	    var iWasCalled = false
	    object TestExtension extends Extension{
	        override def incoming(message: Message) = {
	            iWasCalled = true
	            Some(message)
	        }
	    }
	    
	    TestBayeux.registerExtension(TestExtension)
	    
	    val channel = Channel("/chat/scala")
	    val client = Client.apply
	    
	    val message = new Message(channel = "/chat/scala", clientId = client.uuid)
	    
	    TestBayeux.dispatch(message)
	    
	    iWasCalled must be (true)
	}
	
	it must "apply the outgoing method of the extension when the message is called" in {
	    object TestBayeux extends Bayeux{}
	    var iWasCalled = false
	    object TestExtension extends Extension{
	        override def outgoing(message: Message) = {
	            iWasCalled = true
	            Some(message)
	        }
	    }
	    
	    TestBayeux.registerExtension(TestExtension)
	    
	    val channel = Channel("/chat/scala")
	    val client = Client.apply
	    
	    val message = new Message(channel = "/chat/scala", clientId = client.uuid)
	    
	    TestBayeux.dispatch(message)
	    
	    iWasCalled must be (true)
	}
}