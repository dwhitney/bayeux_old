package us.says.bayeux

//akka
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.collection.HashTrie

//java
import java.util.UUID

object Client{
    
    private var clients = new HashTrie[String, Client]()
    
}

class Client extends Actor{
    
    //create uuid, stripping out the dashes as per the bayeux spec
    override val uuid = UUID.randomUUID.toString.replaceAll("-", "")
    
    //add to the clients hash
    Client.clients = Client.clients.update(uuid, this)
    
    def receive = {
        case _ => println("client received a message")
    }
    
}