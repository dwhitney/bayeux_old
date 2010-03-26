package us.says.bayeux

//joda time
import org.joda.time.DateTime

//akka
import se.scalablesolutions.akka.actor._

//java
import java.util.concurrent.{Future, TimeUnit}

case object Flush{}
case class SetFlusher(flusher: MessageFlusher){}

class MessageFlusher(val messages: List[Message])
	extends Future[List[Message]] 
	with Actor
	with Bayeux{
	
	val created = new DateTime
	
	//all clients should be the same
	val client = if(messages.size > 0 ) messages(0).client else null
	
	//keeps track of when the flusher should flush.  This is essential to the isDone method, explained below;
	private var shouldFlush = false
	
	start
	
	//tell the client that this instance is its MessageFlusher
	client ! SetFlusher(this)
	
	//dispatch all messages from constructor
	messages.foreach{m: Message =>
		dispatch(m) match {
			//enqueue messages from dispatch if responses come back
			case Some(msg: Message) => client ! Enqueue(msg)
			case None => ()
		}
	}

	
	/**
	 * get will always ask the client to flush its messages.
	**/
	def get: List[Message] = get(1000, TimeUnit.MILLISECONDS)
	
	/**
	 * get will always ask the client to flush its messages.
	**/
	def get(timeout: Long, unit: TimeUnit): List[Message] = {
	    shouldFlush = false
	    (client !! (Flush, timeout)).getOrElse(Nil)
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
	 * shouldFlush will be true if the client has signaled to the flusher that it has received a message
	 * that needs to be sent to the client immediately.
	**/
	def isDone = ((new DateTime().getMillis - created.getMillis) > Bayeux.CONNECTION_INTERVAL || shouldFlush)
	
	/**
	 * used to receive messages from the Client for whom this is a MessageFlusher
	**/
	def receive = {
	    //Flush messages come from the client, and are indicative of something that needs to be sent to the client immediately
	    //for example, a publish message like /chat/scala or a handshake message like /meta/handshake
	    //setting shouldFlush to true will cause the isDone method to return true, which will cause tell the thing awaiting on 
	    //this Future that it's readyto send some data
		case Flush => shouldFlush = true
	}
	
}