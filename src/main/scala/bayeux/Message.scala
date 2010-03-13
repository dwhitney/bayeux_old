package us.says.bayeux

//scala
import scala.collection.immutable.HashMap

//joda-time
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

object Message{
    val timestampFormatter = DateTimeFormat.forPattern("YYYY-MM-dd'T'hh:mm:ss")
}

case class Message(val channel: Channel, val client: Client){
    def this(c: Channel) = this(c, null)    
    
    var successful = false;
    var id: String = null;
    var version = Bayeux.VERSION
    var minimumVersion = Bayeux.VERSION
    var error: String = null
    var supportedConnectionTypes = List(Bayeux.LONG_POLLING)
    var connectionType: String = null
    var dateTime: DateTime = new DateTime(DateTimeZone.UTC)
    var timestamp: String = Message.timestampFormatter.print(dateTime)
    
}