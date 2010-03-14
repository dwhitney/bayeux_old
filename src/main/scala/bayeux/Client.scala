package us.says.bayeux

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.collection.HashTrie

//java
import java.util.UUID

object Client{
    
    private var clients = new HashTrie[String, Client]()
    
    def getClients: HashTrie[String, Client] = clients
    def clearClients: Unit = clients = new HashTrie[String, Client]
    
}

case class AddSubscription(channel: Channel){}
case class RemoveSubscription(channel: Channel){}
case object GetSubscriptions{}
case object Disconnect{}
class Client extends Actor{
    
    private var channels = Set[Channel]()
    
    //create uuid, stripping out the dashes as per the bayeux spec
    override val uuid = UUID.randomUUID.toString.replaceAll("-", "")
    
    //start actor by default
    start
    
    //add to the clients hash
    Client.clients = Client.clients.update(uuid, this)
    
    def receive = {
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
    
}