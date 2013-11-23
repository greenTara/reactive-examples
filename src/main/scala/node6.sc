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
                                                  //> EMail1  : Array[Byte] = Array(43, -68)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(119, 92, -18, -97, -84, -32, 94, 33, 79, 121, 
                                                  //| 122)
    
  
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
      case Failure(t) => println(t.toString)
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
      case Failure(t) => println(t.toString)
    }
    confirmation onComplete {
      case Success(t) =>
      case Failure(t) =>
    }
  }                                               //> block: ()Unit
  block()                                         //> java.util.concurrent.ExecutionException: Boxed Error
                                                  //| java.util.concurrent.ExecutionException: Boxed Error
  (1 to 10 toList).foreach(e =>block())           //> Packet Read
                                                  //| Received
                                                  //| java.util.concurrent.ExecutionException: Boxed Error
                                                  //| java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| Packet Read
                                                  //| Received
                                                  //| java.util.concurrent.ExecutionException: Boxed Error
}