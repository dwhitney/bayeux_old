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
	
	/**
	returns a channel of the given name.
	**/
	def apply(name: String): Channel = {
		
		//check if name is valid
		if(name == null || name(0) != '/') throw new IllegalArgumentException("Channels must follow the following paradigm '/foo/bar/baz' and the characters between the slashes must be URL encoded")

        //this method does not support wildcards
        if(name.matches(".*\\*.*")) throw new IllegalArgumentException("the apply method does not support wildcards.  for wildcards see getChannels")

        //last character can't be a /
        if(name(name.length - 1) == '/') throw new IllegalArgumentException("the last character can not be a /")
        
		//make sure the segments are URL encoded
		name.split("/").foreach{ s: String =>
			val pluses = s.replaceAll("%20", "+")
			if(!pluses.equals(URLEncoder.encode(URLDecoder.decode(pluses, "UTF-8"), "UTF-8")))
				throw new IllegalArgumentException("Channels must follow the following paradigm '/foo/bar/baz' and the characters between the slashes must be URL encoded")
		}
		
		//check the channels hash for the channel of the given name
		//if it doesn't exist, create a new one and add it to the hash
		channels.get(name) match {
			case Some(channel: Channel) => channel
			case None =>
				val channel = new Channel(name)
				channels = channels.update(name, channel)
				channel.start
				channel
		}
	}
	
	/**
	Returns a Set of channels.  Wildcards are supported here.  They work as follows:

	The channel patterns support only trailing wildcards of either "*" to match a single segment or "**" to match multiple segments. Example channel patterns are: 
	
    /foo/\*  (that's suppose to be one star, ignore the backslash - stupid comments!)
        Matches /foo/bar and /foo/boo. Does not match /foo, /foobar or /foo/bar/boo.                        
    "/foo/\*\*  (that's two starts, ignore the backslash - stupid comments!)
        Matches /foo/bar, /foo/boo and /foo/bar/boo. Does not match /foo, /foobar or /foobar/boo
        
    @param name the name of the Channel(s) to fetch
    @return a Set of Channels matching the name specified
	**/
	def getChannels(name: String): Set[Channel] = {
	    val channels = all
	    var set = Set[Channel]()
	    
	    //if no wildcard is used, simply fetch the channel from the hash
	    if(!(name(name.length - 1) == '*')){
	        channels.get(name) match {
	            case Some(channel: Channel) => set = set + channel
	            case None => ()
	        }
	    //if multi-line wildcard
	    }else if(name.length >= 3 && name.substring(name.length - 3) == "/**"){
	        val pattern = name.substring(0, name.length - 2) + ".*"
	        channels.keySet.foreach{ key: String => if(key.matches(pattern)) set = set + channels(key) }
	    //single line wildcard
	    }else{
	        val pattern = name.substring(0, name.length - 1) + "[^/]*$"
	        channels.keySet.foreach{ key: String => if(key.matches(pattern)) set = set + channels(key) }
	    }
	    set
	}
	
	def all: HashTrie[String, Channel] = channels

	def clearChannels: Unit = channels = new HashTrie[String, Channel]()

    def main(args: Array[String]) = println("Hello World!")
	
}


case class Subscribe(client: Client){}
case class Unsubscribe(client: Client){}
case object GetSubscribers{}
case class Publish(message: Message){}
class Channel private (n: String) extends Actor{
	
	private var subscriptions = HashTrie[String, Client]()
	
	val name = n
	val segments = n.split("/")
	id = n
	
	def receive = {
	    case Publish(message: Message) =>
            for(key <- subscriptions.keySet) subscriptions(key) ! Publish(message)
	    case Subscribe(client: Client) => 
	        subscriptions = subscriptions + (client.uuid -> client)
	        client ! AddSubscription(this)
	    case Unsubscribe(client: Client) => 
	        subscriptions = subscriptions - client.uuid
	        //client might not be running if this unsubscribe is due to the client getting a Disconnect
	        if(client.isRunning) client ! RemoveSubscription(this)
	    case GetSubscribers => reply(subscriptions)
		case _ => println("message received")
	}
	
}