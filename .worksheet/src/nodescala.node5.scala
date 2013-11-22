package nodescala

import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global


object node5 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(302); 
  println("Welcome to the Scala worksheet");$skip(73); 

    val EMail1 = (for {i <- 0 to 11} yield (random*256).toByte).toArray;System.out.println("""EMail1  : Array[Byte] = """ + $show(EMail1 ));$skip(72); 
    val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
    
  abstract class Confirmation{
    val max: Int
    }
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sentToEurope(packet: Array[Byte]): Future[Array[Byte]]
  };System.out.println("""EMail2  : Array[Byte] = """ + $show(EMail2 ));$skip(238); 
 
  def disconnect(a:Socket) = (random < 0.5)
  class InputException(msg: String) extends Error{
    override def toString = msg
  }
  class TransmissionException(msg: String) extends Error{
    override def toString = msg
  };System.out.println("""disconnect: (a: nodescala.node5.Socket)Boolean""");$skip(201); 
  val maxTotal = 50;System.out.println("""maxTotal  : Int = """ + $show(maxTotal ));$skip(58); 
  
  val Received = "received".map(x => x.toByte).toArray;System.out.println("""Received  : Array[Byte] = """ + $show(Received ));$skip(210); 
   
  def packetSource(rand: Double, prob: Double ): Array[Byte] =
    if (rand < prob) {
      blocking {Thread.sleep(1000)}
      EMail2
    }
    else {
      blocking {Thread.sleep(100)}
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
  };System.out.println("""packetSource: (rand: Double, prob: Double)Array[Byte]""");$skip(537); 

  val socket = Socket();System.out.println("""socket  : nodescala.node5.Socket = """ + $show(socket ));$skip(153); 
 val confirmation: Future[Array[Byte]] = for {
   packet <- socket.readFromMemory()
   confirmation <- socket.sentToEurope(packet)
 } yield confirmation;System.out.println("""confirmation  : scala.concurrent.Future[Array[Byte]] = """ + $show(confirmation ));$skip(48); 
 println(Await.result(confirmation, 10 second))}
   
}
