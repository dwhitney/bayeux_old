package us.says.bayeux

//lift
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonDSL._
import net.liftweb.json.MappingException

//scala
import scala.collection.mutable.Map

//joda-time
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

object Message{
    
    val DATE_FORMAT = "YYYY-MM-dd'T'hh:mm:ss"
    val CHANNEL = "channel"
    val VERSION = "version"
    val SUPPORTED_CONNECTION_TYPES = "supportedConnectionTypes"
    val MINIMUM_VERSION = "minimumVersion"
    val ID = "id"
    val CLIENT_ID = "clientId"
    val SUCCESSFUL = "successful"
    var ERROR = "error"
    
    val timestampFormatter = DateTimeFormat.forPattern(DATE_FORMAT)
    
    implicit def messageToJson(message: Message): JValue = {
        message.channel.name match {
            case Bayeux.META_HANDSHAKE if message.error != null =>
                var json = (CHANNEL -> message.channel.name) ~
                (SUCCESSFUL -> message.successful)
                (ERROR -> message.error)
                (SUPPORTED_CONNECTION_TYPES -> message.supportedConnectionTypes) ~
                (VERSION -> message.version)
                if(message.minimumVersion != null) json = json ~ (MINIMUM_VERSION -> message.minimumVersion)
                if(message.id != null) json = json ~ (ID -> message.id)
                json
            //create a message representing a /meta/connect response
            case Bayeux.META_HANDSHAKE => {
                var json = (CHANNEL -> message.channel.name) ~
                (VERSION -> message.version) ~
                (SUPPORTED_CONNECTION_TYPES -> message.supportedConnectionTypes) ~
                (CLIENT_ID -> message.client.uuid)
                (SUCCESSFUL -> message.successful)
                if(message.minimumVersion != null) json = json ~ (MINIMUM_VERSION -> message.minimumVersion)
                if(message.id != null) json = json ~ (ID -> message.id)
                json
            }
            //
        }
    }
    
    private def extractChannel(json: JValue): Channel = {
        implicit val formats = net.liftweb.json.DefaultFormats
        Channel((json \ Message.CHANNEL).extract[String])
    }
    
    private def extractClient(json: JValue): Client = {
        implicit val formats = net.liftweb.json.DefaultFormats
        val clientId = extractString(json, Message.CLIENT_ID)
        if(clientId != null){
            Client.getClient(clientId).getOrElse(null)
        }else{
            null
        }
    }
    
    private def extractId(json: JValue): String = extractString(json, Message.ID)
    
    //extracts a string from the json with the given fieldName - returns null if not found
    private def extractString(json: JValue, fieldName: String) = {
        implicit val formats = net.liftweb.json.DefaultFormats
        try{ (json \ fieldName).extract[String] } catch { case e: MappingException => null }
    }
    
    
}

case class Message(
        var channel: Channel = null,
        var client: Client = null,
        var connectionType: String = null,
        var data: Map[String, Any] = Map[String, Any](),
        var dateTime: DateTime = new DateTime(DateTimeZone.UTC),
        var error: String = null,
        var id: String = null,
        var minimumVersion: String = Bayeux.VERSION,
        var subscription: Channel = null,
        var successful: Boolean = false,
        var supportedConnectionTypes: List[String] = List(Bayeux.LONG_POLLING),
        var version: String = Bayeux.VERSION){

    def this(json: JValue) = this(
        Message.extractChannel(json),
        Message.extractClient(json))

    var timestamp: String = Message.timestampFormatter.print(dateTime)
    
}