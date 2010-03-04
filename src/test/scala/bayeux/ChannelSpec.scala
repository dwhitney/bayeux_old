package us.says.bayeux

import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import org.scalatest.matchers.MustMatchers

class ChannelSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach{
	
	override def beforeEach: Unit = Channel.clearChannels
	
	"A Channel" should "construct normally" in {
		val channel = Channel("/chat/scala")
		()
	}
	
	it should "throw an IllegalArgumentException if the channel name is null" in {
		evaluating{
			val channel = Channel(null)
			()
		} must produce [IllegalArgumentException]
	}

	
	it should "throw an IllegalArgumentsException when the beginning character isn't a /" in {
		evaluating { 
			val channel = Channel("chat/scala")
			() 
		} must produce [IllegalArgumentException]
	}
	
	it should "throw an IllegalArgumentsException when one of the characters isn't the type allowed in URL Encoding" in {
		evaluating { 
			val channel = Channel("/ch!at/scala")
			() 
		} must produce [IllegalArgumentException]
	}
	
	it should "not throw an IllegalArgumentException when + is in the segments" in {
		val channel = Channel("/ch+at/scala")
		()
	}
	
	it should "not throw an IllegalArgumentException when %20 is in the segments" in {
		val channel = Channel("/ch%20at/scala")
		()
	}
	
	it should "getChannels" in {
		Channel.getChannels must not be (null)
	}
	
	it should "add channels to the channels hash" in {
		Channel.getChannels.size must equal(0)
		val channel = Channel("/chat/scala")
		Channel.getChannels.size must equal(1)
	}
	
	it should "add channels to the channels hash and they should go under the same key" in {
		Channel.getChannels.size must equal(0)
		val channel = Channel("/chat/scala")
		Channel.getChannels.size must equal(1)
		
		val channel2 = Channel("/chat/scala")
		Channel.getChannels.size must equal(1)
	}
	
	it should "add channels to the channels hash getting another should return the exact same instance" in {
		Channel.getChannels.size must equal(0)
		val channel = Channel("/chat/scala")
		Channel.getChannels.size must equal(1)
		
		val channel2 = Channel("/chat/scala")
		assert(channel eq channel2)
	}
	
}