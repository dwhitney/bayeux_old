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
    
    val CHANNEL = "channel"
    val CLIENT_ID = "clientId"
    val CONNECTION_TYPE = "connectionType"
    val DATE_FORMAT = "YYYY-MM-dd'T'hh:mm:ss"
    val ERROR = "error"
    val ID = "id"
    val MINIMUM_VERSION = "minimumVersion"
    val SUBSCRIPTION = "subscription"
    val SUCCESSFUL = "successful"
    val SUPPORTED_CONNECTION_TYPES = "supportedConnectionTypes"
    val TIMESTAMP = "timestamp"
    val VERSION = "version"
    
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
    
    //pulls the channel field out of the json doc and returns a Channel object.  returns null if it isn't found
    private def extractChannel(json: JValue): Channel = {
        implicit val formats = net.liftweb.json.DefaultFormats
        Channel((json \ Message.CHANNEL).extract[String])
    }
    
    //pulls the subscription field out of the json doc and returns a Channel object.  returns null if it isn't found
    private def extractSubscription(json: JValue): Channel = {
        implicit val formats = net.liftweb.json.DefaultFormats
        Channel((json \ Message.SUBSCRIPTION).extract[String])
    }
    
    //pulls the clientId field from the json document and fetches the client with that Id.  returns null if not found
    private def extractClient(json: JValue): Client = {
        implicit val formats = net.liftweb.json.DefaultFormats
        val clientId = extractString(json, Message.CLIENT_ID)
        if(clientId != null){
            Client.getClient(clientId).getOrElse(null)
        }else{
            null
        }
    }
    
    //extracts the id field.  returns null if not found
    private def extractId(json: JValue): String = extractString(json, Message.ID)
    
    //extracts a string from the json with the given fieldName - returns null if not found
    private def extractString(json: JValue, fieldName: String) = {
        implicit val formats = net.liftweb.json.DefaultFormats
        try{ (json \ fieldName).extract[String] } catch { case e: MappingException => null }
    }
    
    //extracts a boolean from the json with the given fieldName - returns false if not found
    private def extractBoolean(json: JValue, fieldName: String) = {
        implicit val formats = net.liftweb.json.DefaultFormats
        try{ (json \ fieldName).extract[Boolean] } catch { case e: MappingException => false }
    }

    private def extractDateTime(json: JValue): DateTime = {
        val timestamp = extractString(json, Message.TIMESTAMP)
        if(timestamp != null) timestampFormatter.withZone(DateTimeZone.UTC).parseDateTime(timestamp)
        else null
    }
    
    import net.liftweb.json.JsonAST._
    private def extractSupportedConnectionTypes(json: JValue): List[String] = for{JString(str) <- (json \ SUPPORTED_CONNECTION_TYPES)} yield str
    
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
        channel = Message.extractChannel(json),
        client = Message.extractClient(json),
        connectionType = Message.extractString(json, Message.CONNECTION_TYPE),
        error = Message.extractString(json, Message.ERROR),
        id = Message.extractString(json, Message.ID),
        minimumVersion = Message.extractString(json, Message.MINIMUM_VERSION),
        subscription = Message.extractSubscription(json),
        successful = Message.extractBoolean(json, Message.SUCCESSFUL),
        version = Message.extractString(json, Message.VERSION),
        dateTime = Message.extractDateTime(json),
        supportedConnectionTypes = Message.extractSupportedConnectionTypes(json))

    var timestamp: String = Message.timestampFormatter.print(dateTime)
    
}