import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global


object node6 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(53, -89)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(-96, -88, 19, -97, -116, -4, -22, -88, 97, -6,
                                                  //|  94)
    
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }
 
  def disconnect(a:Socket) = (random < 0.3)       //> disconnect: (a: node6.Socket)Boolean
  class InputException(msg: String) extends Error{
    override def toString = msg
  }
  class TransmissionException(msg: String) extends Error{
    override def toString = msg
  }
  val maxTotal = 80                               //> maxTotal  : Int = 80
  
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

  def block() = {
    val socket = Socket()
    val packet = socket.readFromMemory()
    Await.ready(packet, 1 second)
    packet onComplete {
      case Success(t) => println("Packet Read")
      case Failure(t: ExecutionException) => println("Error message: " ++ t.getCause().toString)
    }
    packet onComplete {
      case Success(t) =>
      case Failure(t) =>
    }
    val confirmation =
      packet.flatMap(p => {
        socket.sendToEurope(p)
        })
    Await.ready(confirmation, 1 second)
    confirmation onComplete {
      case Success(t) => println("Received")
      case Failure(t: ExecutionException) => println("Error message: " ++ t.getCause().toString)
    }
    confirmation onComplete {
      case Success(t) =>
      case Failure(t) =>
    }
  }                                               //> block: ()Unit
  (1 to 20 toList).foreach(e =>block())           //> Packet Read
                                                  //| Error message: Nice try!
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Packet Read
                                                  //| Received
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Error message: Nice try!
                                                  //| Packet Read
                                                  //| Error message: Nice try!
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Error message: Oooops
                                                  //| Packet Read
                                                  //| Received
}