import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global


object node6 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(283); 
  println("Welcome to the Scala worksheet");$skip(70); 

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray;System.out.println("""EMail1  : Array[Byte] = """ + $show(EMail1 ));$skip(70); 
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
    
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  };System.out.println("""EMail2  : Array[Byte] = """ + $show(EMail2 ));$skip(184); 
 
  def disconnect(a:Socket) = (random < 0.3)
  class InputException(msg: String) extends Error{
    override def toString = msg
  }
  class TransmissionException(msg: String) extends Error{
    override def toString = msg
  };System.out.println("""disconnect: (a: node6.Socket)Boolean""");$skip(201); 
  val maxTotal = 80;System.out.println("""maxTotal  : Int = """ + $show(maxTotal ));$skip(58); 
  
  val Received = "received".map(x => x.toByte).toArray;System.out.println("""Received  : Array[Byte] = """ + $show(Received ));$skip(206); 
   
  def packetSource(rand: Double, prob: Double ): Array[Byte] =
    if (rand < prob) {
      blocking {Thread.sleep(10)}
      EMail2
    }
    else {
      blocking {Thread.sleep(1)}
      EMail1
    }
  
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
  };System.out.println("""packetSource: (rand: Double, prob: Double)Array[Byte]""");$skip(1226); 

  def block() = {
    val socket = Socket()
    val packet = socket.readFromMemory()
    Await.ready(packet, 1 second)
    packet onComplete {
      case Success(t) => println("Packet Read")
      case Failure(t) => println("Error Message: " ++ t.toString)
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
      case Failure(t) => println("Error Message: " ++ t.toString)
    }
    confirmation onComplete {
      case Success(t) =>
      case Failure(t) =>
    }
  };System.out.println("""block: ()Unit""");$skip(40); 
  (1 to 10 toList).foreach(e =>block())}
}
