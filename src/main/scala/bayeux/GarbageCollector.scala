package us.says.bayeux

//akka
import se.scalablesolutions.akka.actor._

//java
import java.util.concurrent.TimeUnit

object GarbageCollector extends Actor{
    
    def receive = {
        case GarbageCollect => 
            ActorRegistry.actorsFor(classOf[Client]) foreach { client: Client => 
                log.debug("inspecting %s", client)
                if(client.isRunning) client ! GarbageCollect
            }
    }
    
}