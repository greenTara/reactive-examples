import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, CanAwait, OnCompleteRunnable, TimeoutException, ExecutionException, blocking }


/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 3, "Combinators on Futures", especially Slide 8.
* The sending commands are made more complicated, so that failure
* can occur in multiple ways. In particular, a sending to Europe
* can fail, or a sending to the USA, or both.
* Using recoverWith and recover, there are fewer failures.
* Note that the effect of this implementation of recoverWith and recover is to hide the
* occurrence of failures of the first sendTo.
* The only error messages that will be printed are Oooops and "Nice Try!",
* never "Guter Versuch!"
* Also note that connection failures (Oooops) are printed twice - why do you
* think that happens?
*/
object node8 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(-59, -121)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(12, 10, -114, 40, -80, 28, -30, 5, 124, 27, -
                                                  //| 123)
  type URL = String
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sendTo(url: URL, packet: Array[Byte]): Future[Array[Byte]]
    def sendToSafe(packet: Array[Byte]): Future[Array[Byte]]
  }
 
  def disconnect(a:Socket) = (random < 0.2)       //> disconnect: (a: node8.Socket)Boolean
  class InputException(msg: String) extends Error{
    override def toString = msg
  }
  class TransmissionException(msg: String) extends Error{
    override def toString = msg
  }
  object mailServer {
  	val europe = "http://example.de"
  	val usa = "http://example.com"
  }
  val maxTotalEurope = 50                         //> maxTotalEurope  : Int = 50
  val multiplierUSA = 2                           //> multiplierUSA  : Int = 2
  
  //Message length 8
  val Received = "Received".map(x => x.toByte).toArray
                                                  //> Received  : Array[Byte] = Array(82, 101, 99, 101, 105, 118, 101, 100)
   
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
       /* The delivery may succeed to both,
       * or one or the other will fail, or both.
       */
       def sendTo(url: URL, packet: Array[Byte]): Future[Array[Byte]] = Future
       { url match {
           case "http://example.de" =>
			         if (packet.length > maxTotalEurope)
			           throw(new TransmissionException("Guter Versuch!"))
			         else
			           Received
           case "http://example.com" =>
			         if (packet.length % multiplierUSA == 0)
			           throw(new TransmissionException("Nice try!"))
			         else
			           Received
			       }
       }
       def sendToSafe(packet: Array[Byte]): Future[Array[Byte]] = {
         sendTo(mailServer.europe, packet) recoverWith {
           case europeError => sendTo(mailServer.usa, packet) recover {
             case usaError  =>
               usaError.getCause().toString.map(x => x.toByte).toArray
           // The code below, as originally given in the lecture, does not
           // demonstrate how a sendTo europe failure is hidden.
           //  case  usaError =>
           //    usaError.getMessage.map(x => x.toByte).toArray
       } } }
       
			
    }
  }

  def block(i:Int) = {
    println("Iteration: " + i.toString)
    val socket = SocketFactory()
    val packet = socket.readFromMemory()
    /* Although the Await.ready method is blocking, the internal use of blocking ensures
    * that the underlying ExecutionContext is prepared to properly manage the blocking.
    * You can uncomment this line to slow down the rate at which new asynchronous
    * computations are spawned by the iteration, a sort of throttling.
    */
    //Await.ready(packet, 1 second)
    packet onComplete {
      case Success(p) => {
        println("Packet Length: " + p.length.toString + " " + i.toString)
      }
      case Failure(t: ExecutionException) => {
        // getCause must be used here to unwrap the Boxed Exception
        println("Error message: " + t.getCause().toString + " " + i.toString)
      }
    }
    val confirmation =
      packet.flatMap(p => {
        socket.sendToSafe( p )
        })
    // Similarly, you may throttle here.
    //Await.ready(confirmation, 1 second)
     /* This command demonstrates that the
      * confirmation Future is available throughout the scope of block()
      * unlike the previous implementation.
      */
    println("Confirmation Ready: " + confirmation.isCompleted.toString + " " + i.toString)
    confirmation onComplete {
      case Success(p) => {
        val s: String = new String(p)
        println("Message: " + s + " " + i.toString)
      }
      case Failure(t: ExecutionException) => {
        println("Error message: " + t.getCause().toString + " " + i.toString)
      }
    }
    
  }                                               //> block: (i: Int)Unit
   /* Multiple executions of a block of commands where
   * each block contains one readFromMemory and, if that is
   * successful, two sendTos, one ot Europe and one to the USA.
   * Note that these blocks execute ansynchronously -
   * the interleaving of the println commands is shown through the iteration index.
   * Also note that the worksheet will typically terminate before the
   * final iteration is complete. So the iteration accomplishes two
   * things - it gives us multiple samples from the random number generators,
   * and it keeps the worksheet functioning long enough to see
   * some of the output of the ansynchronous computations.
   */
  (1 to 15 toList).foreach(i =>block(i))          //> Iteration: 1
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
                                                  //| Confirmation Ready: false 8
                                                  //| Iteration: 9
                                                  //| Confirmation Ready: false 9
                                                  //| Error message: Oooops 5
                                                  //| Iteration: 10
                                                  //| Error message: Oooops 5
                                                  //| Confirmation Ready: false 10
                                                  //| Iteration: 11
                                                  //| Confirmation Ready: false 11
                                                  //| Iteration: 12
                                                  //| Confirmation Ready: false 12
                                                  //| Iteration: 13
                                                  //| Confirmation Ready: false 13
                                                  //| Iteration: 14
                                                  //| Confirmation Ready: false 14
                                                  //| Iteration: 15
                                                  //| Confirmation Ready: false 15
   //keeps the worksheet alive so the iterations can finish!
  blocking{Thread.sleep(3000)}                    //> Error message: Oooops 15
                                                  //| Error message: Oooops 15
                                                  //| Packet Length: 47 4
                                                  //| Message: Received 4
                                                  //| Packet Length: 56 9
                                                  //| Packet Length: 56 8
                                                  //| Packet Length: 56 2
                                                  //| Packet Length: 56 13
                                                  //| Message: Nice try! 2
                                                  //| Message: Nice try! 13
                                                  //| Message: Nice try! 9
                                                  //| Message: Nice try! 8
                                                  //| Packet Length: 65 11
                                                  //| Message: Received 11
                                                  //| Packet Length: 65 7
                                                  //| Message: Received 7
                                                  //| Packet Length: 65 14
                                                  //| Message: Received 14
                                                  //| Packet Length: 74 3
                                                  //| Packet Length: 74 6
                                                  //| Message: Nice try! 3
                                                  //| Message: Nice try! 6
                                                  //| Packet Length: 74 1
                                                  //| Message: Nice try! 1
                                                  //| Packet Length: 74 10
                                                  //| Message: Nice try! 10
                                                  //| Packet Length: 92 12
                                                  //| Message: Nice try! 12/
  
}