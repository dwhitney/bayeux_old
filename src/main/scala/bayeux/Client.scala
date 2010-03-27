package us.says.bayeux

//joda time
import org.joda.time.DateTime

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.stm.HashTrie

//java
import java.util.UUID

//scala
import scala.collection.immutable.Queue

object Client{
    
    private var clients = new HashTrie[String, Client]()
    
    def getClients: HashTrie[String, Client] = clients
    def getClient(uuid: String): Option[Client] = clients.get(uuid)
    def clearClients: Unit = clients = new HashTrie[String, Client]
    
    def apply: Client = {
        val client = new Client
        client.start
        println("Client Created: " + client)
        client
    }
    
}

case class AddSubscription(channel: Channel){}
case class RemoveSubscription(channel: Channel){}
case object GetSubscriptions{}
case object Disconnect{}
case object GetMessageQueue{}
case class Enqueue(message: Message){}
case object GarbageCollect{}

class Client private() extends Actor{
    
    private var channels = Set[Channel]()
    private var messageQueue: Queue[Message] = Queue[Message]()
	private var flusher: MessageFlusher = null
	private var shouldFlush: Boolean = false
    private var lastMetaConnect: DateTime = new DateTime
    
    //create uuid, stripping out the dashes as per the bayeux spec
    override val uuid = UUID.randomUUID.toString.replaceAll("-", "")
    
    //add to the clients hash
    Client.clients = Client.clients.update(uuid, this)
    
    log.debug("created %s", this)
    
    def receive = {
		case SetFlusher(f: MessageFlusher) => 
			flusher = f
			if(shouldFlush) flush
		case Enqueue(message: Message) => processMessage(message)
		case Flush => 
		    val messages = messageQueue.toList
		    messageQueue = Queue[Message]()
		    reply(messages)
        case GetMessageQueue => reply(messageQueue)
        case AddSubscription(channel: Channel) =>
            //checking to see if the channel is already subscribed to, because Channel will call AddSubscription when a 
            //client is subscribed, and we want to avoid infinite calls back and forth
            if(!channels.contains(channel)){
                channels = channels + channel
                channel ! Subscribe(this)
            }
        case RemoveSubscription(channel: Channel) =>
            //checking to see if the channel is already removed, because Channel will call RemoveSubscription when a 
            //client is unsubscribed, and we want to avoid infinite calls back and forth
            if(channels.contains(channel)){
                channels = channels - channel
                channel ! Unsubscribe(this)
            }
        //if this client has been around longer than the Bayeux.TIMEOUT_VALUE + 10 seconds, then stop it
        case GarbageCollect =>
            if((new DateTime().getMillis - lastMetaConnect.getMillis) > (Bayeux.TIMEOUT_VALUE + 10000)) this ! Disconnect
        case GetSubscriptions => reply(channels)
        case Disconnect =>
            //unsubscribe to all channels and call stop
            channels.foreach(_ ! Unsubscribe(this))
            stop
        case _ => println("client received an unknown message")
    }



	//this will enqueue every message sent this way, and determine if the messageQueue should be flushed to the flusher, if one exists
	private def processMessage(message: Message): Unit = {
		messageQueue = messageQueue enqueue message
		message.channel.name match {
			case Bayeux.META_SUBSCRIBE => flush
			case Bayeux.META_HANDSHAKE => flush
			case Bayeux.META_DISCONNECT => flush
			case Bayeux.META_CONNECT if message.id != null && message.id.toString.matches("[1234]") => 
			    //if this is the second message from the client, it's probably the first connect message, and we should 
			    //flush immediatly because that's what the jquery client seems to want?... dunno why
			    lastMetaConnect = new DateTime
			    message.advice = Bayeux.DEFAULT_ADVICE
			    flush
			case Bayeux.META_CONNECT => 
			    lastMetaConnect = new DateTime
			    ()
			case Bayeux.META_SUBSCRIBE => ()
			case Bayeux.META_UNSUBSCRIBE => ()
			case _ => flush
		}
	}
	
	//either tells the flusher to ask for a flush, or sets the shouldFlush flag to true,
	//so when a flusher is set, it will automatically ask it to flush
	private def flush: Unit = {
		if(flusher != null){
		    flusher ! Flush
		    //set the flusher to null, because it's going to get stopped.
		    flusher = null
		    shouldFlush = false
	    }else shouldFlush = true  
	}
    
}