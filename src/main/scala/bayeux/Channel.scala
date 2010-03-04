package us.says.bayeux

//java
import java.net.{URLEncoder, URLDecoder}

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.state.TransactionalState
import se.scalablesolutions.akka.collection.HashTrie
import se.scalablesolutions.akka.stm.Transaction._

object Channel{
	
	private var channels = new HashTrie[String, Channel]()
	
	def apply(name: String): Channel = {
		
		//check if name is valid
		if(name == null || name(0) != '/') 
			throw new IllegalArgumentException("Channels must follow the following paradigm '/foo/bar/baz' and the characters between the slashes must be URL encoded")

		//make sure the segments are URL encoded
		name.split("/").foreach{ s: String =>
			val pluses = s.replaceAll("%20", "+")
			if(!pluses.equals(URLEncoder.encode(URLDecoder.decode(pluses))))
				throw new IllegalArgumentException("Channels must follow the following paradigm '/foo/bar/baz' and the characters between the slashes must be URL encoded")
		}
		
		channels.get(name) match {
			case Some(channel: Channel) => channel
			case None =>
				val channel = new Channel(name)
				channels = channels + (name -> channel)
				channel
		}
	}
	
	def getChannels: HashTrie[String, Channel] = channels
	def clearChannels: Unit = channels = new HashTrie[String, Channel]()
	
}

class Channel private (n: String) extends Actor{
	
	val name = n
	val segments = n.split("/")
	id = n
	
	def receive = {
		case _ => println("message received")
	}
	
}