package nodescala

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
                                                  //> EMail1  : Array[Byte] = Array(-30, 58)
    val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(-126, -59, 123, 34, 53, -34, 0, -74, -62, -2, 
                                                  //| -73)
    
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sentToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }
 
  def disconnect(a:Socket) = (random < 0.3)       //> disconnect: (a: nodescala.node5.Socket)Boolean
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
       
       def sentToEurope(packet: Array[Byte]): Future[Array[Byte]] = Future
       {
         if (packet.length > maxTotal)
           throw(new TransmissionException("Nice try!"))
         else
           Received
       }
    }
  }

  def block() = Future {
    val socket = Socket()
		 val confirmation: Future[Array[Byte]] = for {
		   packet <- socket.readFromMemory()
		   confirmation <- socket.sentToEurope(packet)
		 } yield confirmation
     confirmation onComplete {
       case Success(cf) => println("Confirmation: " ++ cf.toString)
       case Failure(t) => println("Error: " ++ t.toString)
     }
  }                                               //> block: ()scala.concurrent.Future[Unit]
  
  (1 to 10 toList).foreach(e =>
    Await.result(block(), 1 second)
  )                                               //> Error: java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Error: java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Error: java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Error: java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Error: java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Error: java.util.concurrent.ExecutionException: Boxed Error
                                                  //| Error: java.util.concurrent.ExecutionException: Boxed Error
}