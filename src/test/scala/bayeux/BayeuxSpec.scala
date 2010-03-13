package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers
import se.scalablesolutions.akka.collection.HashTrie

class BayeuxSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = Channel.clearChannels
	
	"A Bayeux" should "include the handshake channel when responding to a handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.channel must equal(Channel(Bayeux.META_HANDSHAKE))
		()
	}
	
	it must "include a version when responding to a handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.version must equal (Bayeux.VERSION)
	}
	
	it must "include supported connection types when responding to a handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.supportedConnectionTypes must equal (Bayeux.SUPPORTED_CONNECTION_TYPES)
	}
	
	it must "include a newly created client when responding to a handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        println(response.error)
        response.client must not be (null)
	}

    it must "include successful=true responding to a handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        val response = TestBayeux.dispatch(message).get
        response.successful must be (true)
	}
	
	it must "include the same id as the one included in the handshake responding to a handshake" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        message.id = "myId"
        val response = TestBayeux.dispatch(message).get
        response.id must equal (message.id)
	}
	
	it must "create an error message when a version is not included in a handshake from client" in {
	    object TestBayeux extends Bayeux{}
        val message = new Message(Channel(Bayeux.META_HANDSHAKE))
        message.version = null
        val response = TestBayeux.dispatch(message).get
        response.error must equal(String.format("%s:%s:the version specified is incompatible with this implementation of Bayeux", Bayeux.ERROR_INVALID_VERSION.toString, null))
	}
	
	
}