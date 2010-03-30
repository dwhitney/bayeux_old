package us.says.bayeux

trait Extension{
    
    /**
     * Called when the extension is registered
    **/
    def registered: Unit = ()
    
    /**
     * Called when the extension is unregistered
    **/
    def unregistered: Unit = ()
    
    /**
     * Called on all incoming messages.
     * @param message this is the incoming message.
     * @return Return None if you'd like to stop the message
    **/
    def incoming(message: Message): Option[Message] = Some(message)
    
    /**
     * Called on all outgoing messages.
     * @param message this is the outgoing message.
     * @return Return None if you'd like to stop the message
    **/
    def outgoing(message: Message): Option[Message] = Some(message)
    
}