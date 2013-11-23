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
                                                  //> EMail1  : Array[Byte] = Array(-54, -117)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(101, 67, 48, 99, 9, -85, -98, 82, -93, 44, 57)
                                                  //| 
    
  
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
  def block(i:Int) = {
    println("Iteration: " ++ i.toString)
    val socket = Socket()
    val packet = socket.readFromMemory()
    Await.ready(packet, 1 second)

    packet onComplete {
      case Success(p) => {
        println("Packet Read: " ++ i.toString)
  		  val confirmation: Future[Array[Byte]] =  socket.sendToEurope(p)
  		  Await.ready(confirmation, 1 second)
        println("Confirmation Ready: " ++ i.toString)
        confirmation onComplete {
          case Success(cf) => println("Confirmation: Received " ++ i.toString)
          case Failure(t: ExecutionException) => println("Error message: " ++ t.getCause().toString)
        }
		    confirmation onComplete {
		      case Success(p) =>
		  		case Failure(t) =>
		    }
  		}
      case Failure(t: ExecutionException) => {
        println("Error message: " ++ t.getCause().toString)
      }
    }
    packet onComplete {
      case Success(p) =>
  		case Failure(t) =>
    }
    
   }                                              //> block: (i: Int)Unit
   (1 to 5 toList).foreach(i =>block(i))          //> Iteration: 1
                                                  //| Error message: Oooops
                                                  //| Iteration: 2
                                                  //| Iteration: 3
                                                  //| Packet Read: 2
                                                  //| Confirmation Ready: 2
                                                  //| Error message: Nice try!
                                                  //| Packet Read: 3
                                                  //| Iteration: 4
                                                  //| Confirmation Ready: 3
                                                  //| Confirmation: Received 3
                                                  //| Iteration: 5
                                                  //| Packet Read: 4
}