import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global


object node5 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(283); 
  println("Welcome to the Scala worksheet");$skip(70); 

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray;System.out.println("""EMail1  : Array[Byte] = """ + $show(EMail1 ));$skip(70); 
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
    
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sentToEurope(packet: Array[Byte]): Future[Array[Byte]]
  };System.out.println("""EMail2  : Array[Byte] = """ + $show(EMail2 ));$skip(182); 
 
  def disconnect(a:Socket) = (random < 0)
  class InputException(msg: String) extends Error{
    override def toString = msg
  }
  class TransmissionException(msg: String) extends Error{
    override def toString = msg
  };System.out.println("""disconnect: (a: node5.Socket)Boolean""");$skip(201); 
  val maxTotal = 50;System.out.println("""maxTotal  : Int = """ + $show(maxTotal ));$skip(58); 
  
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
       
       def sentToEurope(packet: Array[Byte]): Future[Array[Byte]] = Future
       {
         if (packet.length > maxTotal)
           throw(new TransmissionException("Nice try!"))
         else
           Received
       }
    }
  };System.out.println("""packetSource: (rand: Double, prob: Double)Array[Byte]""");$skip(539); 

    val socket = Socket();System.out.println("""socket  : node5.Socket = """ + $show(socket ));$skip(41); 
    val packet = socket.readFromMemory();System.out.println("""packet  : scala.concurrent.Future[Array[Byte]] = """ + $show(packet ));$skip(34); val res$0 = 
    Await.ready(packet, 1 second);System.out.println("""res0: node5.packet.type = """ + $show(res$0));$skip(17); val res$1 = 
    packet.value;System.out.println("""res1: Option[scala.util.Try[Array[Byte]]] = """ + $show(res$1));$skip(23); val res$2 = 
    packet.isCompleted;System.out.println("""res2: Boolean = """ + $show(res$2));$skip(59); 
    packet.onFailure {case t => println("No Packet Read")};$skip(56); 
    packet.onSuccess {case t => println("Packet Read")};$skip(412); 
    packet onComplete {
      case Success(p) => {
        println("Packet Read")
  		  val confirmation: Future[Array[Byte]] =  socket.sentToEurope(p)
        confirmation onComplete {
          case Success(cf) => println("Confirmation: " ++ cf.toString)
          case Failure(t) => println("Error: " ++ t.toString)
        }
  		}
  		case Failure(t) => {
  		          println("No Packet Read")
  		}
    }}
}
