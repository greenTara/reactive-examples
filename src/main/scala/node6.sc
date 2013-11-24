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
                                                  //> EMail1  : Array[Byte] = Array(15, 24)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(4, -64, 21, 42, 47, -71, -8, -6, 52, 7, 118)
    
  
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
  
  object SocketFactory {
    /* The anonymous class syntax is used for this factory object,
    * allowing us to instantiate an object
    * that extends a trait having undefined members.
    * The anonymous class must provide definitions
    * for all undefined members of the trait.
    * Note that this object has an apply method:
    * SocketFactory() is desugared to
    * SocketFactory.apply()
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
  }

  def block(i:Int) = {
    println("Iteration: " + i.toString)
    val socket = SocketFactory()
    val packet = socket.readFromMemory()
    /* Although the Await.ready method is blocking, the internal use of blocking ensures
    * that the underlying ExecutionContext is prepared to properly manage the blocking.
    * You can uncomment this line to slow down the rate at which new asynchronous
    * computations are spawned by the iteration.
    */
    //Await.ready(packet, 1 second)
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
                                                  //| Error message: Oooops 1
                                                  //| Testing: true 1
                                                  //| Error message: Oooops 1
                                                  //| Iteration: 2
                                                  //| Testing: false 2
                                                  //| Iteration: 3
                                                  //| Testing: false 3
                                                  //| Iteration: 4
                                                  //| Testing: false 4
                                                  //| Iteration: 5
                                                  //| Testing: false 5
                                                  //| Iteration: 6
                                                  //| Testing: false 6
                                                  //| Iteration: 7
                                                  //| Testing: false 7
                                                  //| Iteration: 8
                                                  //| Error message: Oooops 7
                                                  //| Error message: Oooops 7
                                                  //| Testing: false 8
   //keeps the worksheet alive so the iterations can finish!
  blocking{Thread.sleep(3000)}                    //> Packet Read: 5
                                                  //| Received 5
                                                  //| Packet Read: 8
                                                  //| Received 8
                                                  //| Received 2
                                                  //| Packet Read: 2
                                                  //| Packet Read: 3
                                                  //| Received 3
                                                  //| Packet Read: 6
                                                  //| Received 6
                                                  //| Packet Read: 4
                                                  //| Error message: Nice try! 4/
  
}