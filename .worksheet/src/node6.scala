import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global

/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 3, "Combinators on Futures", especially Slide 4.
* The use of flatMap gets us around the nested ansynchronous calls of the
* previous implementation.
* However, note that we still have to case match for the potential failures of the Futures,
* and the Exception that we get in such a case is a "Boxed Exception" that must be
* unwrapped using "getCause" in order for it to provide useful feedback.
*/
object node6 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(762); 
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
  
  val Received = "received".map(x => x.toByte).toArray;System.out.println("""Received  : Array[Byte] = """ + $show(Received ));$skip(391); 
   
  def packetSource(rand: Double, prob: Double ): Array[Byte] =
    if (rand < prob) {
        /*Blocking designates a piece of code that potentially blocks,
      * allowing the thread scheduler to add additional threads and
      * resolve potential deadlocks.
      */
      blocking {Thread.sleep(10)}
      EMail2
    }
    else {
      blocking {Thread.sleep(1)}
      EMail1
    }
  
  object MySocket {
    /* The anonymous class syntax is used here,
    * allowing us to instantiate an object
    * that extends a trait with undefined members.
    */
    def apply() = new Socket {
       def readFromMemory(): Future[Array[Byte]] = Future {
         if (disconnect(this))
           throw(new InputException("Oooops"))
           /* This usage of higher-order functions
           * employs the "flattening" effect of flatMap
           * to concatenate the Array[Byte]s of the individual
           * emails into one Array[Byte] that is the packet.
           */
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
  };System.out.println("""packetSource: (rand: Double, prob: Double)Array[Byte]""");$skip(2210); 

  def block(i:Int) = {
    println("Iteration: " + i.toString)
    val socket = MySocket()
    val packet = socket.readFromMemory()
     /* Although the Await.ready method is blocking, the internal use of blocking ensures
    * that the underlying ExecutionContext is prepared to properly manage the blocking.
    */
    Await.ready(packet, 1 second)
    packet onComplete {
      case Success(t) => {
        println("Packet Read: " + i.toString)
      }
      case Failure(t: ExecutionException) => {
        // getCause must be used here to unwrap the Boxed Exception
        println("Error message: " + t.getCause().toString + " " + i.toString)
      }
    }
    val confirmation =
      packet.flatMap(p => {
        socket.sendToEurope(p)
        })
    Await.ready(confirmation, 1 second)
    confirmation onComplete {
      case Success(t) => {
        println("Received " + i.toString)
      }
      case Failure(t: ExecutionException) => {
        println("Error message: " + t.getCause().toString + " " + i.toString)
      }
    }
     /* This command demonstrates that the
      * confirmation Future is available throughout the scope of block()
      * unlike the previous implementation.
      */
     println("Testing: " + confirmation.isCompleted.toString + " " + i.toString)
    
  };System.out.println("""block: (i: Int)Unit""");$skip(671); 
   /* Multiple executions of a block of commands where
   * each block contains one readFromMemory and, if that is
   * successful, one sendToEurope.
   * Note that these blocks execute ansynchrounsly -
   * the interleaving of the println commands is shown through the iteration index.
   * Also note that the worksheet will typically terminate before the
   * final iteration is complete. So the iteration accomplishes two
   * things - it gives us multiple samples from the random number generators,
   * and it keeps the worksheet functioning long enough to see
   * some of the output of the ansynchronous computations.
   */
  (1 to 8 toList).foreach(i =>block(i))}
}
