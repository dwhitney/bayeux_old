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
	private var shouldFlush = false
	
	start
	
	message.client ! SetFlusher(this)
	message.client ! Enqueue(message)
	
	def cancel(mayInterruptIfRunning: Boolean): Boolean = false
	def get: List[Message] = Nil
	def get(timeout: Long, unit: TimeUnit) = Nil
	def isCancelled: Boolean = false
	def isDone = ((new DateTime().getMillis - created.getMillis) > 10000 || shouldFlush)
	
	def receive = {
		case Flush => shouldFlush = true
	}
	
}