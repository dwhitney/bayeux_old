package us.says.bayeux

object Bayeux{
    
    val META_SUBSCRIBE = "/meta/subscribe"
    val META_HANDSHAKE = "/meta/hanshake"
    val META_CONNECT = "/meta/connect"
    val VERSION = "1.0.0"
    val LONG_POLLING = "long-polling"
    val SUPPORTED_CONNECTION_TYPES = List(LONG_POLLING)
    
    
    //error codes - don't know if there is a canonical set
    val ERROR_INVALID_VERSION = 401
    val ERROR_UNSUPPORTED_CONNECTION_TYPES = 402
    val ERROR_MISSING_CLIENT_ID = 403
    val ERROR_MISSING_CONNECTION_TYPE = 404
    val ERROR_UNSUPPORTED_CONNECTION_TYPE = 405
}

trait Bayeux{
    
    //dispatches messages
    def dispatch(message: Message): Option[Message] = {
        message.channel.name match {
            case Bayeux.META_HANDSHAKE => metaHandshake(message)
            case Bayeux.META_SUBSCRIBE => metaSubscribe(message)
            case Bayeux.META_CONNECT => metaConnect(message)
            case _ => None
        }
    }
    
    //logic necessary to carry out a connection
    private def metaConnect(message: Message): Option[Message] = {
        message match {
            //test for missing clientId
            case m: Message if(m.client == null) =>
                error(message,
                    List[Int](Bayeux.ERROR_MISSING_CLIENT_ID),
                    List[String](null),
                    "either a clientId was not sent, or it was not found")
            //test for missing connectionType
            case m: Message if(m.connectionType == null) =>
                error(message,
                    List[Int](Bayeux.ERROR_MISSING_CONNECTION_TYPE),
                    List[String](null),
                    "a connectionType was not specified")
            //test for upsupported connectionType
            case m: Message if(!Bayeux.SUPPORTED_CONNECTION_TYPES.contains(m.connectionType)) =>
                error(message,
                    List[Int](Bayeux.ERROR_UNSUPPORTED_CONNECTION_TYPE),
                    List[String](m.connectionType),
                    "the connectionType specified is unsupported")
            //error free request state
            case _ =>
                val response = Message(message.channel, message.client)
                response.successful = true
                response.id = message.id
                Some(response)
        }
    }
    
    //logic necessary to carry out a handshake
    private def metaHandshake(message: Message): Option[Message] = {
        
        message match {
            //test for incompatible version
            case m: Message if(m.version != Bayeux.VERSION) => 
                error(message, 
                    List[Int](Bayeux.ERROR_INVALID_VERSION),
                    List[String](message.version),
                    "the version specified is incompatible with this implementation of Bayeux")
            //test for supported connection types
            case m: Message if(!m.supportedConnectionTypes.contains(Bayeux.SUPPORTED_CONNECTION_TYPES.head)) =>
                error(message, 
                    List[Int](Bayeux.ERROR_UNSUPPORTED_CONNECTION_TYPES),
                    message.supportedConnectionTypes,
                    "none of the supported connection types match those supported by this implementation of Bayeux")
            //valid message
            case _ =>
                val response = new Message(Channel(Bayeux.META_HANDSHAKE), new Client)
                response.successful = true
                response.id = message.id
                Some(response)
        }
        
    }
    
    private def metaSubscribe(message: Message): Option[Message] = Some(message)
    
    //creates an error message with the given copy
    private def error(message: Message, codes: List[Int], args: List[String], copy: String): Option[Message] = {
        val errorResponse = new Message(message.channel)
        errorResponse.id = message.id
        errorResponse.successful = false
        errorResponse.error = String.format("%s:%s:%s", codes.mkString(" "), args.mkString(","), copy)
        Some(errorResponse)
    }
    
}