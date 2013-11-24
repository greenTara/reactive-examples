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
object node6 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(-75, 41)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(120, 15, -43, -8, 12, 35, 65, -79, -105, 25, -
                                                  //| 2)
    
  
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
    }                                             //> packetSource: (rand: Double, prob: Double)Array[Byte]
  
  object Socket {
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
  }

  def block(i:Int) = {
    println("Iteration: " + i.toString)
    val socket = Socket()
    val packet = socket.readFromMemory()
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
    
  }                                               //> block: (i: Int)Unit
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
  (1 to 8 toList).foreach(i =>block(i))           //> Iteration: 1
                                                  //| Packet Read: 1
                                                  //| Testing: true 1
                                                  //| Error message: Nice try! 1
                                                  //| Iteration: 2
                                                  //| Packet Read: 2
                                                  //| Testing: true 2
                                                  //| Received 2
                                                  //| Iteration: 3
                                                  //| Error message: Oooops 3
                                                  //| Error message: Oooops 3
                                                  //| Testing: true 3
                                                  //| Iteration: 4
                                                  //| Error message: Oooops 4
                                                  //| Error message: Oooops 4
                                                  //| Testing: true 4
                                                  //| Iteration: 5
                                                  //| Error message: Oooops 5
                                                  //| Error message: Oooops 5
                                                  //| Testing: true 5
                                                  //| Iteration: 6
                                                  //| Packet Read: 6
                                                  //| Testing: true 6
                                                  //| Received 6
                                                  //| Iteration: 7
                                                  //| Error message: Oooops 7
                                                  //| Error message: Oooops 7
                                                  //| Testing: true 7
                                                  //| Iteration: 8
                                                  //| Packet Read: 8
                                                  //| Received 8
                                                  //| Testing: true 8
}