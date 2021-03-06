package us.says.bayeux

//joda time
import org.joda.time.DateTime

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.remote._
import se.scalablesolutions.akka.stm._
import se.scalablesolutions.akka.stm.Transaction.Local._
import se.scalablesolutions.akka.util.UUID

//java
import java.util.concurrent.{Future, TimeUnit}

//scala
import scala.collection.immutable.Queue

object Client{
    
    private lazy val ids = TransactionalState.newRef[Int]
    
    private val parts = "(.*?):(.*?):(.*?)".r
    
    def getClient(id: String): Option[Actor] = {
        ActorRegistry.actorsFor(id) match {
            case client :: Nil => Some(client.asInstanceOf[Client])
            case client :: tail => Some(client.asInstanceOf[Client]) //this should never happen
            case Nil => 
                val client = new Client(Some(id))
                client.start
                Some(client)
        }
    }
    
    def apply: Client = {
        val client = new Client(None)
        client.start
        client
    }
    
    def nextId: Int = {
        atomic{
            val id = ids.get.getOrElse(0) + 1
            ids.swap(id)
        }
    }
    
}

case class AddSubscription(name: String){}
case class RemoveSubscription(name: String){}
case object GetSubscriptions{}
case object Disconnect{}
case object GetMessageQueue{}
case class Enqueue(message: Message){}
case object GarbageCollect{}
case object Flush{}
case object IsDone{}

class Client private (clientId: Option[String])
    extends Actor
    with Future[List[Message]]{
    
    private var channels = Set[Channel]()
    private var messageQueue: Queue[Message] = Queue[Message]()
	private var shouldFlush: Boolean = false
    private var lastMetaConnect: DateTime = new DateTime
    
    //create uuid, stripping out the dashes as per the bayeux spec
    override val uuid = clientId match {
        case Some(str) => str
        case None => UUID.newUuid.toString
    }
    id = uuid
    
    log.debug("Client Created %s", this)
    
    
    /**
	 * get will always ask the client to flush its messages.
	**/
	def get: List[Message] = get(1000, TimeUnit.MILLISECONDS)
	
	/**
	 * get will always ask the client to flush its messages.  we use message passing here to avoid concurrency issues
	**/
	def get(timeout: Long, unit: TimeUnit): List[Message] = {
	    if(this.isRunning) (this !! (Flush, timeout)).getOrElse(Nil)
	    else Nil
	}
	
	/**
	 * always returns false.  will change if I see a reason to.
	**/
	def isCancelled: Boolean = false
	
	/**
	 * always returns false.  will change if I see a reason to.
	**/
	def cancel(mayInterruptIfRunning: Boolean): Boolean = false
	
	/**
	 * This method will only return true if shouldFlush is true, or the timeout has been reached.
	**/
	def isDone = {
	    if(this.isRunning)(this !! (IsDone, 1000)).getOrElse(false)
	    else true
	}
    
    def receive = {
        case IsDone =>
            val done = (((new DateTime().getMillis - lastMetaConnect.getMillis) > Bayeux.TIMEOUT_VALUE) || shouldFlush)
            if(done){
                log.debug("Client %s IsDone", this)
                shouldFlush = false
            }
            reply(done)
		case Enqueue(message: Message) => 
		    log.debug("Client %s received Enqueue(Client %s)", this, message)
		    processMessage(message)
		case Flush => 
		    log.debug("Client %s received Flush", this)
		    val messages = messageQueue.toList
		    messageQueue = Queue[Message]()
		    reply(messages)
        case GetMessageQueue => 
            log.debug("Client %s received GetMessageQueue")
            reply(messageQueue)
        case AddSubscription(name: String) =>
            log.debug("Client %s received AddSubscription(Client %s)", this, name)
            //checking to see if the channel is already subscribed to, because Channel will call AddSubscription when a 
            //client is subscribed, and we want to avoid infinite calls back and forth
            val channel = Channel(name)
            if(!channels.contains(channel)){
                channels = channels + channel
                channel ! Subscribe(this.uuid)
            }
        case RemoveSubscription(name: String) =>
            log.debug("Client %s received RemoveSubscription(Client %s)", this, name)
            val channel = Channel(name)
            //checking to see if the channel is already removed, because Channel will call RemoveSubscription when a 
            //client is unsubscribed, and we want to avoid infinite calls back and forth
            if(channels.contains(channel)){
                channels = channels - channel
                channel ! Unsubscribe(this.uuid)
            }
        //if this client has been around longer than the Bayeux.TIMEOUT_VALUE + 10 seconds, then stop it
        case GarbageCollect =>
            if((new DateTime().getMillis - lastMetaConnect.getMillis) > (Bayeux.TIMEOUT_VALUE + 10000)){
                this ! Disconnect
                log.debug("Client %s has been GarbageCollect'd", this)
            }    
        case GetSubscriptions => 
            log.debug("Client %s received GetSubscriptions", this)
            reply(channels)
        case Disconnect =>
            log.debug("Client %s received Disconnect", this)
            //unsubscribe to all channels and call stop
            channels.foreach(_ ! Unsubscribe(this.uuid))
            stop
        case _ => println("client received an unknown message")
    }



	//this will enqueue every message sent this way, and determine if the messageQueue should be flushed
	private def processMessage(message: Message): Unit = {
		messageQueue = messageQueue enqueue message
		message.channel match {
			case Bayeux.META_SUBSCRIBE => flush
			case Bayeux.META_HANDSHAKE => flush
			case Bayeux.META_DISCONNECT => 
			    messageQueue = Queue[Message]() //a disconnect message is immediately sent to the client via the Bayeux.dispatch() message, so we clear the queue here, since nothing else needs to go to the client
			    flush
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
	
	private def flush: Unit = shouldFlush = true
    
}