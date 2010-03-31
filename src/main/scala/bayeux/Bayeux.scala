package us.says.bayeux

//akka
import se.scalablesolutions.akka.actor._  

object Bayeux{
    
    val META_SUBSCRIBE = "/meta/subscribe"
    val META_UNSUBSCRIBE = "/meta/unsubscribe"
    val META_HANDSHAKE = "/meta/handshake"
    val META_CONNECT = "/meta/connect"
    val META_DISCONNECT = "/meta/disconnect"
    val VERSION = "1.0"
    val LONG_POLLING = "long-polling"
    val SUPPORTED_CONNECTION_TYPES = List(LONG_POLLING)
    val TIMEOUT = "timeout"
	var TIMEOUT_VALUE = 1000 * 30 * 1 //one minute
	val INTERVAL = "interval"
	val INTERVAL_VALUE = 0
	val RECONNECT = "reconnect"
	val RETRY = "retry"
	val DEFAULT_ADVICE = Map(INTERVAL -> INTERVAL_VALUE, RECONNECT -> RETRY, TIMEOUT -> TIMEOUT_VALUE)
    
    
    //error codes - don't know if there is a canonical set
    val ERROR_INVALID_VERSION = 401
    val ERROR_UNSUPPORTED_CONNECTION_TYPES = 402
    val ERROR_MISSING_CLIENT_ID = 403
    val ERROR_MISSING_CONNECTION_TYPE = 404
    val ERROR_UNSUPPORTED_CONNECTION_TYPE = 405
    val ERROR_NO_SUBSCRIPTION_SPECIFIED = 406
    val ERROR_NO_CHANNEL_SPECIFIED = 407
    val ERROR_MISSING_DATA_FIELD = 408
    val ERROR_SUBSCRIPTION_TO_META_CHANNEL = 409
    val ERROR_SUBSCRIPTION_TO_SERVICE_CHANNEL = 410
}

trait Bayeux{
    
    //list of extensions
    private var extensions: List[Extension] = Nil
    
    /**
     * registers an extension
     * @param the extensions you'd like to register
    **/
    def registerExtension(extension: Extension): Unit = {
        extensions.synchronized{
            extensions = extension :: extensions
        }
        extension.registered
    }
    
    /**
     * unregisters an extension
     * @param the extension to unregister
    **/
    def unregisterExtension(extension: Extension): Unit = {
        extensions.synchronized{
            extensions = extensions.filterNot(_ == extension)
        }
        extension.unregistered
    }
    
    //dispatches messages
    def dispatch(message: Message): Option[List[Message]] = {
        if(message.channel == null){
            error(message, List[Int](Bayeux.ERROR_NO_CHANNEL_SPECIFIED),List[String](null),"no channel was specified")
        }else{
            
            //function that recusively applies all of the extensions to the message
            def runExtensions(message: Option[Message], extensions: List[Extension], incoming: Boolean): Option[Message] = {
                message match {
                    case Some(_) =>
                        extensions match {
                            case Nil => message
                            case e :: tail => 
                                if(incoming) runExtensions(e.incoming(message.get), tail, incoming)
                                else runExtensions(e.outgoing(message.get), tail, incoming)
                        }
                    case None => None
                }
            }
            
            //apply each extension's incoming method to the incoming methods, then dispatch the results
            val messages = runExtensions(Some(message), extensions.synchronized{ extensions }, true) match {
                case Some(message) => 
                    message.channel.name match {
                        case Bayeux.META_CONNECT => metaConnect(message)
                        case Bayeux.META_SUBSCRIBE => metaSubscribe(message)
                        case Bayeux.META_UNSUBSCRIBE => metaUnsubscribe(message)
                        case Bayeux.META_HANDSHAKE => metaHandshake(message)
                        case Bayeux.META_DISCONNECT => metaDisconnect(message)
                        case _ => publish(message)
                    }
                case None => None
            }
            
            //apply each extension's outgoing method to the messages returned above, which are in turn returned by the method
            messages.getOrElse(Nil).flatMap{m: Message => runExtensions(Some(m), extensions.synchronized{ extensions }, false)} match {
                case Nil => None
                case list: List[Message] => Some(list)
            }
        }
    }
    
    //event messages, where most messages go
    private def publish(message: Message): Option[List[Message]] = {
        message.channel ! Publish(message)
        val ack = Message(isResponse = true,
                channel = message.channel,
                successful = true, 
                id = message.id, 
                data = message.data,
                ext = message.ext)
        //immediately respond so that we do not have to close an already open connection
        val response = Message(
            channel = message.channel,
            client = message.client,
            data = message.data,
            id = message.id,
            ext = message.ext
        )
        Some(List(ack, response)) 
    }
    
    //logic to carry out a /meta/unsubscribe message
    private def metaUnsubscribe(message: Message): Option[List[Message]] = {
        message match {
            //check for missing clientId
            case m: Message if(m.client == null) => missingClient(m)
            //check for missing subsciption channel
            case m: Message if(m.subscription == null) => 
                error(message,
                    List[Int](Bayeux.ERROR_NO_SUBSCRIPTION_SPECIFIED),
                    List[String](null),
                    "no subscription was specified")
                    
            //valid state
            case _ =>
                message.subscription ! Unsubscribe(message.client)
                val response = Message(channel = message.channel,
                    client = message.client,
                    successful = true,
                    subscription = message.subscription,
                    id = message.id,
                    isResponse = true)
                Some(List(response))
        }
    }
    
    //logic to carry out a /meta/subscribe message
    private def metaSubscribe(message: Message): Option[List[Message]] = {
        message match {
            //check for missing clientId
            case m: Message if(m.client == null) => missingClient(m)
            //check for missing subsciption channel
            case m: Message if(m.subscription == null) => 
                error(message,
                    List[Int](Bayeux.ERROR_NO_SUBSCRIPTION_SPECIFIED),
                    List[String](null),
                    "no subscription was specified")
            case m: Message if(m.subscription.name.matches("^\\/meta\\/.*")) =>
                error(message,
                    List[Int](Bayeux.ERROR_SUBSCRIPTION_TO_META_CHANNEL),
                    List[String](null),
                    "you attempted to subscribe to a meta channel")
            case m: Message if(m.subscription.name.matches("^\\/service\\/.*")) =>
                error(message,
                    List[Int](Bayeux.ERROR_SUBSCRIPTION_TO_SERVICE_CHANNEL),
                    List[String](null),
                    "you attempted to subscribe to a service channel")
            //valid state
            case _ =>
                message.subscription ! Subscribe(message.client)
                val response = new Message(
                    channel = message.channel, 
                    client = message.client,
                    successful = true,
                    subscription = message.subscription,
                    id = message.id,
                    isResponse = true)
                Some(List(response))
        }
    }
    
    //logic to carry out a /meta/disconnect message
    private def metaDisconnect(message: Message): Option[List[Message]] = {
        message match {
            //test for missing clientId
            case m: Message if(m.client == null) => missingClient(m)
            //valid state
            case _ =>
                message.client ! Disconnect
                val response = new Message(channel = message.channel, 
                        client = message.client,
                        successful = true,
                        id = message.id,
                        isResponse = true)
                Some(List(response))
                
        }
    }
    
    //logic necessary to carry out a /meta/connect message
    private def metaConnect(message: Message): Option[List[Message]] = {
        message match {
            //test for missing clientId
            case m: Message if(m.client == null) => missingClient(m)
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
            //error free request state.  Enqueue message, and return None, because /meta/connect messages aren't returned immediately
            case _ =>
                val response = new Message(
                        channel = message.channel, 
                        client = message.client,
                        successful = true,
                        id = message.id,
                        isResponse = true)
                        
                message.client ! Enqueue(response)
                None
        }
    }
    
    //logic necessary to carry out a /meta/handshake message
    private def metaHandshake(message: Message): Option[List[Message]] = {
        
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
                val response = new Message(
                    channel = Channel(Bayeux.META_HANDSHAKE), 
                    client = Client.apply,
                    successful = true,
                    id = message.id,
                    advice = Bayeux.DEFAULT_ADVICE,
                    isResponse = true)
                Some(List(response))
        }
        
    }
    
    //common error response for missing client
    private def missingClient(message: Message): Option[List[Message]] = error(message,
        List[Int](Bayeux.ERROR_MISSING_CLIENT_ID),
        List[String](null),
        "either a clientId was not sent, or it was not found")
    
    //creates an error message with the given copy
    private def error(message: Message, codes: List[Int], args: List[String], copy: String): Option[List[Message]] = {
        val errorResponse = new Message(isResponse = true,
            channel = message.channel,
            id = message.id,
            successful = false,
            error = String.format("%s:%s:%s", codes.mkString(" "), args.mkString(","), copy))
        Some(List(errorResponse))
    }
    
}