import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global


object node5 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(-101, 107)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(-68, -94, 50, -14, 46, 34, 28, -50, -103, 84, 
                                                  //| -91)
    
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }
 
  def disconnect(a:Socket) = (random < 0.3)       //> disconnect: (a: node5.Socket)Boolean
  class InputException(msg: String) extends Error{
    override def toString = msg
  }
  class TransmissionException(msg: String) extends Error{
    override def toString = msg
  }
  val maxTotal = 50                               //> maxTotal  : Int = 50
  
  val Received = "received".map(x => x.toByte).toArray
                                                  //> Received  : Array[Byte] = Array(114, 101, 99, 101, 105, 118, 101, 100)
   
  def packetSource(rand: Double, prob: Double ): Array[Byte] =
    if (rand < prob) {
      blocking {Thread.sleep(10)}
      EMail2
    }
    else {
      blocking {Thread.sleep(1)}
      EMail1
    }                                             //> packetSource: (rand: Double, prob: Double)Array[Byte]
  
  object Socket {
    def apply() = new Socket {
       def readFromMemory(): Future[Array[Byte]] = Future {
         if (disconnect(this))
           throw(new InputException("Oooops"))
         else (1 to 10 toArray) flatMap(i => packetSource(random, 0.5))
       }
       
       def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] = Future
       {
         if (packet.length > maxTotal)
           throw(new TransmissionException("Nice try!"))
         else
           Received
       }
    }
  }

    val socket = Socket()                         //> socket  : node5.Socket = node5$$anonfun$main$1$Socket$3$$anon$1@77a172dc
    val packet = socket.readFromMemory()          //> packet  : scala.concurrent.Future[Array[Byte]] = scala.concurrent.impl.Prom
                                                  //| ise$DefaultPromise@609fc98
    Await.ready(packet, 1 second)                 //> res0: node5.packet.type = scala.concurrent.impl.Promise$DefaultPromise@609f
                                                  //| c98
    packet.value                                  //> res1: Option[scala.util.Try[Array[Byte]]] = Some(Success([B@1daadbf7))
    packet.isCompleted                            //> res2: Boolean = true

    packet onComplete {
      case Success(p) => {
        println("Packet Read")
  		  val confirmation: Future[Array[Byte]] =  socket.sendToEurope(p)
  		  Await.ready(confirmation, 1 second)
        println("Confirmation Ready")
        confirmation onComplete {
          case Success(cf) => println("Confirmation: " ++ cf.toString)
          case Failure(t) => println("Error: " ++ t.toString)
        }
		    packet onComplete {
		      case Success(p) =>
		  		case Failure(t) =>
		    }
  		}
  		case Failure(t) => {
  		          println("No Packet Read")
  		}
    }
    packet onComplete {
      case Success(p) =>
  		case Failure(t) =>
    }                                             //> Packet Read
}