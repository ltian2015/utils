import akka.actor._
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
object FaultHandlingDocSample extends App{
  import actors._
  import Worker._
  import akka.event.LoggingReceive
  val config = ConfigFactory.parseString("""
  akka.loglevel = "DEBUG"
  akka.actor.debug {
  receive = on
  lifecycle = on
  }
  """)
  val system = ActorSystem("FaultToleranceSample", config)
  val worker = system.actorOf(Props[Worker], name = "worker")
  val listener = system.actorOf(Props[Listener], name = "listener")
  // start the work and listen on progress
  // note that the listener is used as sender of the tell,
  // i.e. it will receive replies from the worker
  worker.tell(Start, sender = listener) 
    
}