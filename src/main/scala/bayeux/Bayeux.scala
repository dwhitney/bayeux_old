package us.says.bayeux

object Bayeux{
    
    val META_SUBSCRIBE = "/meta/subscribe"
    val META_HANDSHAKE = "/meta/hanshake"
    val VERSION = "1.0.0"
    val LONG_POLLING = "long-polling"
    val SUPPORTED_CONNECTION_TYPES = List(LONG_POLLING)    
    
    //error codes - don't know if there is a canonical set
    val ERROR_INVALID_VERSION = 401
    val ERROR_UNSUPPORTED_CONNECTION_TYPES = 402
}

trait Bayeux{
    
    //dispatches messages
    def dispatch(message: Message): Option[Message] = {
        message.channel.name match {
            case Bayeux.META_HANDSHAKE => metaHandshake(message)
            case Bayeux.META_SUBSCRIBE => metaSubscribe(message)
            case _ => None
        }
    }
    
    //logic necessary for carrying out a handshake
    private def metaHandshake(message: Message): Option[Message] = {
        
        message match {
            case m: Message if(m.version != Bayeux.VERSION) => 
                error(message, 
                    List[Int](Bayeux.ERROR_INVALID_VERSION),
                    List[String](message.version),
                    "the version specified is incompatible with this implementation of Bayeux")
            case m: Message if(!m.supportedConnectionTypes.contains(Bayeux.SUPPORTED_CONNECTION_TYPES.head)) =>
                error(message, 
                    List[Int](Bayeux.ERROR_UNSUPPORTED_CONNECTION_TYPES),
                    message.supportedConnectionTypes,
                    "none of the supported connection types match those supported by this implementation of Bayeux")
            case _ => //valid message
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
        errorResponse.error = String.format("%s:%s:%s", codes.mkString(" "), args.mkString(","), copy)
        Some(errorResponse)
    }
    
}