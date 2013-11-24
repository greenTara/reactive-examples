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
                                                  //> EMail1  : Array[Byte] = Array(36, -16)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(14, 65, -10, 75, -99, 54, 102, -117, 91, 14, -
                                                  //| 19)
    
  
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
    //Await.ready(confirmation, 1 second)
     /* This command demonstrates that the
      * confirmation Future is available throughout the scope of block()
      * unlike the previous implementation.
      */
    println("Confirmation Ready: " + confirmation.isCompleted.toString + " " + i.toString)
    confirmation onComplete {
      case Success(t) => {
        println("Received " + i.toString)
      }
      case Failure(t: ExecutionException) => {
        println("Error message: " + t.getCause().toString + " " + i.toString)
      }
    }
    
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
  (1 to 10 toList).foreach(i =>block(i))          //> Iteration: 1
                                                  //| Confirmation Ready: false 1
                                                  //| Iteration: 2
                                                  //| Confirmation Ready: false 2
                                                  //| Iteration: 3
                                                  //| Confirmation Ready: false 3
                                                  //| Iteration: 4
                                                  //| Confirmation Ready: false 4
                                                  //| Iteration: 5
                                                  //| Confirmation Ready: false 5
                                                  //| Iteration: 6
                                                  //| Confirmation Ready: false 6
                                                  //| Iteration: 7
                                                  //| Confirmation Ready: false 7
                                                  //| Iteration: 8
                                                  //| Error message: Oooops 2
                                                  //| Error message: Oooops 2
                                                  //| Error message: Oooops 4
                                                  //| Error message: Oooops 7
                                                  //| Error message: Oooops 7
                                                  //| Confirmation Ready: false 8
                                                  //| Error message: Oooops 4
                                                  //| Error message: Oooops 8
                                                  //| Iteration: 9
                                                  //| Error message: Oooops 8
                                                  //| Error message: Oooops 9
                                                  //| Confirmation Ready: false 9
                                                  //| Iteration: 10
                                                  //| Error message: Oooops 9
                                                  //| Confirmation Ready: false 10
   //keeps the worksheet alive so the iterations can finish!
  blocking{Thread.sleep(3000)}                    //> Packet Read: 10
                                                  //| Received 10
                                                  //| Packet Read: 6
                                                  //| Received 6
                                                  //| Packet Read: 5
                                                  //| Received 5
                                                  //| Packet Read: 1
                                                  //| Received 1
                                                  //| Packet Read: 3
                                                  //| Error message: Nice try! 3/
  
}