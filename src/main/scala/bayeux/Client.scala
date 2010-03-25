package us.says.bayeux

//scala
import scala.collection.immutable.Queue

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.collection.HashTrie

//java
import java.util.UUID

object Client{
    
    private var clients = new HashTrie[String, Client]()
    
    def getClients: HashTrie[String, Client] = clients
    def getClient(uuid: String): Option[Client] = clients.get(uuid)
    def clearClients: Unit = clients = new HashTrie[String, Client]
    
}

case class AddSubscription(channel: Channel){}
case class RemoveSubscription(channel: Channel){}
case object GetSubscriptions{}
case object Disconnect{}
case object GetMessageQueue{}
case class Enqueue(message: Message){}

class Client extends Actor{
    
    private var channels = Set[Channel]()
    private var messageQueue: Queue[Message] = Queue[Message]()
	private var flusher: MessageFlusher = null
	private var shouldFlush: Boolean = false
    
    //create uuid, stripping out the dashes as per the bayeux spec
    override val uuid = UUID.randomUUID.toString.replaceAll("-", "")
    
    //start actor by default
    start
    
    //add to the clients hash
    Client.clients = Client.clients.update(uuid, this)
    
    def receive = {
		case SetFlusher(f: MessageFlusher) => 
			this.flusher = f
			if(shouldFlush){
				flusher ! Flush
				shouldFlush = false
			}
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
			case Bayeux.META_CONNECT => ()
			case Bayeux.META_SUBSCRIBE => ()
			case Bayeux.META_UNSUBSCRIBE => ()
			case _ => flush
		}
	}
	
	//either tells the flusher to ask for a flush, or sets the shouldFlush flag to true,
	//so when a flusher is set, it will automatically ask it to flush
	private def flush: Unit = {
		if(flusher != null) flusher ! Flush
		else shouldFlush = true
	}
    
}