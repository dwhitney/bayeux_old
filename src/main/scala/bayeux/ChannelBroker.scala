package us.says.bayeux

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.stm.TransactionalState
import se.scalablesolutions.akka.stm.HashTrie
import se.scalablesolutions.akka.stm.Transaction._
import se.scalablesolutions.akka.remote.Cluster

//java
import java.util.UUID

//commons
import org.apache.commons.codec.digest.DigestUtils

case class GetChannel(name: String){}
case object GetChannels{}
case class ChannelExists(name: String){}
case class AddChannelBroker(broker: ChannelBroker){}
case class ChannelCreated(channel: Channel){}
case class AddChannel(channel: Channel){}
case object ClearChannels{}

object ChannelBroker{
    lazy val broker = new ChannelBroker
}


class ChannelBroker private() extends Actor{
    
    override val uuid = UUID.randomUUID.toString.replaceAll("-", "")
    id = uuid
    
    private var channels = Map[String, Channel]()
    private var brokers = List(this)
    
    start
    
    Cluster.relayMessage(classOf[ChannelBroker], AddChannelBroker(this))
        
    
    def receive = {
        case GetChannel(name) => reply(channels.getOrElse(name, getChannel(name)))
        case ChannelExists(name) => {
            println("Channel exists: " + name + " -- " + channels.get(name))
            
            reply(channels.get(name))
        }
        case ChannelCreated(channel) => 
            channels = channels.updated(channel.name, channel)
            Cluster.relayMessage(classOf[ChannelBroker], AddChannel(channel))
        case AddChannelBroker(broker) => 
            brokers.find(_.uuid == this.uuid) match {
                case None => 
                    brokers = broker :: brokers
                    brokers = brokers.sortWith(_.uuid < _.uuid)
                case _ => ()
            }
        case AddChannel(channel) => channels = channels.updated(channel.name, channel)
        case ClearChannels => channels = Map[String, Channel]()
        case GetChannels => reply(channels)
    }
    
    
    private def getChannel(name: String): Channel = {
        def brokerIndex(channelName: String) = {
    	    val md5 = DigestUtils.md5Hex(channelName.getBytes("UTF-8"))
    	    val ALPHABET_SIZE = 16 //16 is the number of possible values the first digit can be since we're using hex
    	    
            val firstDigit = md5(0) match {
                case '0' => 0
                case '1' => 1
                case '2' => 2
                case '3' => 3
                case '4' => 4
                case '5' => 5
                case '6' => 6
                case '7' => 7
                case '8' => 8
                case '9' => 9
                case 'a' => 10
                case 'b' => 11
                case 'c' => 12
                case 'd' => 13
                case 'e' => 14
                case 'f' => 15
            }
            (firstDigit * brokers.size / ALPHABET_SIZE) % ALPHABET_SIZE 
        }
        
        val broker = brokers(brokerIndex(name))
        
        if(broker.uuid == this.uuid) Channel(name)
        else (broker !! (GetChannel(name), 1000)).getOrElse(Channel(name))
    }
}