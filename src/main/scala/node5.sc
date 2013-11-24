import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global

/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 2, "Latency as an Effect", particularly slide 17.
* This approach to implementing futures is not recommended, because
* of the messy nesting that happens around line 94.
* In particular, the confirmation object is local to the onComplete
* block, so the results are difficult (impossible?) to work with outside of that scope.
*/

object node5 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(0, 115)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(-62, 103, -68, 94, -7, 3, 71, -59, 58, -71, 18
                                                  //| )
    
  
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
    /* The anonymous class syntax is used here,
    * allowing us to create a companion object
    * for a trait with undefined members.
    */
    def apply() = new Socket {
       def readFromMemory(): Future[Array[Byte]] = Future {
         if (disconnect(this))
           throw(new InputException("Oooops"))
         else
           /* This usage of higher-order functions
           * employs the "flattening" effect of flatMap
           * to concatenate the Array[Byte]s of the individual
           * emails into one Array[Byte] that is the packet.
           */
           (1 to 10 toArray) flatMap(i => packetSource(random, 0.5))
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
      case Success(p) => {
        println("Packet Read: " + i.toString)
        // messy nesting starts here
  		  val confirmation: Future[Array[Byte]] =  socket.sendToEurope(p)
  		  Await.ready(confirmation, 1 second)
        println("Testing: " + confirmation.isCompleted.toString + " " + i.toString)
        println("Confirmation Ready: " + i.toString)
        confirmation onComplete {
          case Success(cf) => {
            println("Confirmation: Received " + i.toString)
          }
          case Failure(t: ExecutionException) => {
            // getCause must be used here to unwrap the Boxed Exception
            println("Error message: " + t.getCause().toString + " " + i.toString)
          }
        }
  		}
      case Failure(t: ExecutionException) => {
        println("Error message: " + t.getCause().toString + " " + i.toString)
      }
      /* Uncommenting this line gives an error message, demonstrating that the
      * confirmation Future is local to the scope of the onComplete call.
      */
      //  println("Testing: " + confirmation.isCompleted.toString + " " + i.toString)
    }
    
   }                                              //> block: (i: Int)Unit
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
   (1 to 8 toList).foreach(i =>block(i))          //> Iteration: 1
                                                  //| Iteration: 2
                                                  //| Packet Read: 1
                                                  //| Testing: true 1
                                                  //| Confirmation Ready: 1
                                                  //| Error message: Nice try! 1
                                                  //| Iteration: 3
                                                  //| Packet Read: 2
                                                  //| Iteration: 4
                                                  //| Error message: Oooops 3
                                                  //| Testing: true 2
                                                  //| Confirmation Ready: 2
                                                  //| Error message: Nice try! 2
                                                  //| Iteration: 5
                                                  //| Packet Read: 4
                                                  //| Testing: true 4
                                                  //| Confirmation Ready: 4
                                                  //| Error message: Nice try! 4
                                                  //| Iteration: 6
                                                  //| Packet Read: 5
                                                  //| Error message: Oooops 6
                                                  //| Testing: true 5
                                                  //| Iteration: 7
                                                  //| Confirmation Ready: 5
                                                  //| Error message: Nice try! 5
                                                  //| Iteration: 8
                                                  //| Packet Read: 7
                                                  //| Testing: true 7
                                                  //| Confirmation Ready: 7
                                                  //| Error message: Nice try! 7
}