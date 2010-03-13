package us.says.bayeux

//scala
import scala.collection.immutable.HashMap

case class Message(val channel: Channel, val client: Client){
    def this(c: Channel) = this(c, null)    
    
    var successful = false;
    var id: String = null;
    var version = Bayeux.VERSION
    var minimumVersion = Bayeux.VERSION
    var error: String = null
    var supportedConnectionTypes = List(Bayeux.LONG_POLLING)
    
}