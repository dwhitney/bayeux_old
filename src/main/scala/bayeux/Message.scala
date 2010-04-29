package us.says.bayeux

//lift
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.MappingException

//joda-time
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

object Message{
    
    val ADVICE = "advice"
    val CHANNEL = "channel"
    val CLIENT_ID = "clientId"
    val CONNECTION_TYPE = "connectionType"
    val DATA = "data"
    val DATE_FORMAT = "YYYY-MM-dd'T'hh:mm:ss"
    val ERROR = "error"
    val EXT = "ext"
    val ID = "id"
    val MINIMUM_VERSION = "minimumVersion"
    val SUBSCRIPTION = "subscription"
    val SUCCESSFUL = "successful"
    val SUPPORTED_CONNECTION_TYPES = "supportedConnectionTypes"
    val TIMESTAMP = "timestamp"
    val VERSION = "version"
    
    val timestampFormatter = DateTimeFormat.forPattern(DATE_FORMAT)
      
    //transforms a map into a JObject
    implicit def mapToJObject(map: Map[String, Any]): JObject = {
        import net.liftweb.json.JsonAST._
        import net.liftweb.json.JsonDSL._
        import net.liftweb.json.Implicits._
        
        def transform(m: Map[String, Any]): JObject = JObject(m.map{ case (k: String, x) => JField(k, getField(x)) }.toList)
            
        def getField(any: Any): JValue = {
            any match {
                case a: Boolean => JBool(a)
                case a: Double => JDouble(a)
                case a: Int => JInt(a)
                case a: String => JString(a)
                case a: List[Any] => JArray(a.map(getField))
                case null => JNull
                case a: Map[String, Any] => transform(a)
                case a => JString(a.toString)
            }
        }
        
        transform(map)
    }
    
    //transforms a message into json
    implicit def messageToJson(message: Message): JObject = {
        import net.liftweb.json.JsonAST
		import net.liftweb.json.JsonAST._
        import net.liftweb.json.JsonDSL._

		//assuming CHANNEL exists, which it should
        var json: JObject = (CHANNEL -> message.channel)
        if(message.clientId != null) json = json ~ (CLIENT_ID -> message.clientId)
        if(message.connectionType != null) json = json ~ (CONNECTION_TYPE -> message.connectionType)
        if(message.dateTime != null) json = json ~ (TIMESTAMP -> message.timestamp)
        if(message.error != null) json = json ~ (ERROR -> message.error)
        if(message.id != null) json = json ~ (ID -> message.id)
        if(message.subscription != null) json = json ~ (SUBSCRIPTION -> message.subscription.name)
        if(message.isResponse) json = json ~ (SUCCESSFUL -> message.successful)
        if(message.ext.size > 0) json = json ~ (EXT -> message.ext)
        if(message.advice.size > 0) json = json ~ (ADVICE -> message.advice)
        
        message.channel match {
            case Bayeux.META_HANDSHAKE => 
                json = json ~ (VERSION -> message.version) ~ (SUPPORTED_CONNECTION_TYPES -> message.supportedConnectionTypes)
                if(message.minimumVersion != null) json = json ~ (MINIMUM_VERSION -> message.minimumVersion)
                //add advice
                ()
            case Bayeux.META_CONNECT => () //add advice
            case Bayeux.META_DISCONNECT => () //add advice
            case Bayeux.META_SUBSCRIBE => () //add advice
            case Bayeux.META_UNSUBSCRIBE => () //add advice
            case _ => //add advice
                json = json ~ (DATA -> message.data)
            
        }
        
        //data, ext, supportedConnectionTypes, version, minimumVersion
        json
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

    //gets the DateTime from Json
    private def extractDateTime(json: JValue): DateTime = {
        val timestamp = extractString(json, Message.TIMESTAMP)
        if(timestamp != null) timestampFormatter.withZone(DateTimeZone.UTC).parseDateTime(timestamp)
        else null
    }
    
    //extracts the data map.  returns an empty if a map isn't found
    private def extractData(json: JValue): Map[String, Any] = {
        import net.liftweb.json.JsonParser._
        (json \ DATA) match {
            case JField(DATA, obj: JObject) => obj.values
            case _ => Map[String, Any]()
        }
    }
    
    //extracts the ext map.  returns an empty if a map isn't found
    private def extractExt(json: JValue): Map[String, Any] = {
        import net.liftweb.json.JsonParser._
        (json \ EXT) match {
            case JField("ext", obj: JObject) => obj.values
            case _ => Map[String, Any]()
        }
    }
    
    import net.liftweb.json.JsonAST._
    private def extractSupportedConnectionTypes(json: JValue): List[String] = for{JString(str) <- (json \ SUPPORTED_CONNECTION_TYPES)} yield str
    
}

case class Message(
        var advice: Map[String, Any] = Map[String, Any](),
        val channel: String = null,
        val clientId: String = null,
        val connectionType: String = null,
        var data: Map[String, Any] = Map[String, Any](),
        val dateTime: DateTime = new DateTime(DateTimeZone.UTC),
        val error: String = null,
        var ext: Map[String, Any] = Map[String, Any](),
        val id: String = null,
        val isResponse: Boolean = false,
        val minimumVersion: String = Bayeux.VERSION,
        val subscription: String = null,
        val successful: Boolean = false,
        val supportedConnectionTypes: List[String] = List(Bayeux.LONG_POLLING),
        val version: String = Bayeux.VERSION){

    def this(json: JValue) = this(
        channel = Message.extractString(json, Message.CHANNEL),
        clientId = Message.extractString(json, Message.CLIENT_ID),
        connectionType = Message.extractString(json, Message.CONNECTION_TYPE),
        error = Message.extractString(json, Message.ERROR),
        ext = Message.extractExt(json),
        id = Message.extractString(json, Message.ID),
        minimumVersion = Message.extractString(json, Message.MINIMUM_VERSION),
        subscription = Message.extractString(json, Message.SUBSCRIPTION),
        successful = Message.extractBoolean(json, Message.SUCCESSFUL),
        version = Message.extractString(json, Message.VERSION),
        dateTime = Message.extractDateTime(json),
        supportedConnectionTypes = Message.extractSupportedConnectionTypes(json),
        data = Message.extractData(json))

    val timestamp: String = Message.timestampFormatter.print(dateTime)
    
}