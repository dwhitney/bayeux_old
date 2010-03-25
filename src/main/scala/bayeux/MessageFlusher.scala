package us.says.bayeux

//joda time
import org.joda.time.DateTime

//akka
import se.scalablesolutions.akka.actor._

//java
import java.util.concurrent.{Future, TimeUnit}

case object Flush{}
case class SetFlusher(flusher: MessageFlusher){}

class MessageFlusher(val message: Message)
	extends Future[List[Message]] 
	with Actor{
	
	val created = new DateTime
	
	//keeps track of when the flusher should flush.  This is essential to the isDone method, explained below;
	private var shouldFlush = false
	
	start
	
	//tell the client that this instance is its MessageFlusher
	message.client ! SetFlusher(this)
	message.client ! Enqueue(message)

	
	/**
	 * get will always ask the client to flush its messages.
	**/
	def get: List[Message] = get(1000, TimeUnit.MILLISECONDS)
	
	/**
	 * get will always ask the client to flush its messages.
	**/
	def get(timeout: Long, unit: TimeUnit): List[Message] = {
	    shouldFlush = false
	    (message.client !! (Flush, timeout)).getOrElse(Nil)
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
	def isDone = ((new DateTime().getMillis - created.getMillis) > 10000 || shouldFlush)
	
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