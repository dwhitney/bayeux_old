package us.says.bayeux

//java
import java.net.{URLEncoder, URLDecoder}

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.remote._
import se.scalablesolutions.akka.stm.TransactionalState
import se.scalablesolutions.akka.stm.HashTrie
import se.scalablesolutions.akka.stm.Transaction._

object Channel{
	
	//private var channels = new HashTrie[String, Channel]()
	
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
		
		val channels = ActorRegistry.actorsFor(name)
		channels match {
			case channel :: Nil => channel.asInstanceOf[Channel]
			case channel :: tail =>
			    //log.warning("there are two channels with the same name.  this really shouldn't happen")
			    channel.asInstanceOf[Channel]
			case Nil =>
				val channel = new Channel(name)
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
	    
	    var set: Set[Channel] = Set[Channel]()
	    
	    //if no wildcard is used, simply fetch the channel from the hash
	    if(!(name(name.length - 1) == '*')){
	        val channels = ActorRegistry.actorsFor(name)
	        channels match {
	            case channel :: Nil => set = set + channel.asInstanceOf[Channel]
	            case channel :: tail =>
    			    //log.warning("there are two channels with the same name.  this really shouldn't happen")
    			    channel.asInstanceOf[Channel]
	            case Nil => ()
	        }
	    //if multi-line wildcard
	    }else if(name.length >= 3 && name.substring(name.length - 3) == "/**"){
	        val pattern = name.substring(0, name.length - 2) + ".*"
	        val channels = ActorRegistry.actorsFor[Channel]
	        //log.warning("wildcard searches are likely to be inefficient in a clustered environment")
	        channels.foreach{ channel: Channel => if(channel.name.matches(pattern)) set = set + channel }
	    //single line wildcard
	    }else{
	        val channels = ActorRegistry.actorsFor[Channel]
	        val pattern = name.substring(0, name.length - 1) + "[^/]*$"
	        //log.warning("wildcard searches are likely to be inefficient in a clustered environment")
	        channels.foreach{ channel: Channel => if(channel.name.matches(pattern)) set = set + channel }
	    }
	    set
	}
	
}


case class Subscribe(clientId: String){}
case class Unsubscribe(clientId: String){}
case object GetSubscribers{}
case class Publish(message: Message){}
class Channel private (n: String) extends Actor{
	
	private var subscriptions = HashTrie[String, Client]()
	
	val name = n
	override val uuid = name
	val segments = n.split("/")
	id = n
	
	override def init: Unit = RemoteNode.register(uuid, this)
	
	def receive = {
	    case Publish(message: Message) =>
	        log.debug("Channel %s received Publish(%s)", this, message)
            for(key <- subscriptions.keySet) if(subscriptions(key).uuid != message.clientId) subscriptions(key) ! Enqueue(message)
	    case Subscribe(clientId: String) => 
	        log.debug("Channel %s received Subscribe(%s)", this, clientId)
	        Client.getClient(clientId) match {
	            case Some(client: Client) =>
	                subscriptions = subscriptions + (clientId -> client)
        	        client ! AddSubscription(this.name)
        	    case None => ()
	        }
	    case Unsubscribe(clientId: String) => 
	        log.debug("Channel %s received Unsubscribe(%s)", this, clientId)
	        subscriptions = subscriptions - clientId
	        //client might not be running if this unsubscribe is due to the client getting a Disconnect
	        Client.getClient(clientId) match {
	            case Some(client: Client) =>
	                if(client.isRunning) client ! RemoveSubscription(this.name)
	            case None => ()
	        }
	    case GetSubscribers => 
	        log.debug("Channel %s received GetSubscribers", this)
	        reply(subscriptions)
	}
	
}