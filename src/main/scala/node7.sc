import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global

/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 3, "Combinators on Futures", especially Slide 6.
* The sending commands are made more complicated, so that failure
* can occur in multiple ways. In particular, a sending to Europe
* can fail, or a sending to the USA, or both.
* Although a straight-forward modification of the previous flatMap method
* does catch the exceptions, it considers it a failure if either sending
* fails. Also, only the first occuring failure is reported.
*/
object node7 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(-126, -107)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(115, -63, -55, -43, 109, 126, -15, -21, -26, 2
                                                  //| 9, -11)
  type URL = String
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sendTo(url: URL, packet: Array[Byte]): Future[Array[Byte]]
    def sendToAndBackup(packet: Array[Byte]): Future[(Array[Byte], Array[Byte])]
  }
 
  def disconnect(a:Socket) = (random < 0.1)       //> disconnect: (a: node7.Socket)Boolean
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
  val maxTotalEurope = 70                         //> maxTotalEurope  : Int = 70
  val multiplierUSA = 4                           //> multiplierUSA  : Int = 4
  
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
       def sendToAndBackup(packet: Array[Byte]): Future[(Array[Byte], Array[Byte])] = {
         val europeConfirm = sendTo(mailServer.europe, packet)
         val usaConfirm = sendTo(mailServer.usa, packet)
         europeConfirm.zip(usaConfirm)
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
    Await.ready(packet, 1 second)
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
        socket.sendToAndBackup( p )
        })
    Await.ready(confirmation, 1 second)
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
   * successful, two sendTos, one ot Europe and one to the USA.
   * Note that these blocks execute ansynchronously -
   * the interleaving of the println commands is shown through the iteration index.
   * Also note that the worksheet will typically terminate before the
   * final iteration is complete. So the iteration accomplishes two
   * things - it gives us multiple samples from the random number generators,
   * and it keeps the worksheet functioning long enough to see
   * some of the output of the ansynchronous computations.
   */
  (1 to 10 toList).foreach(i =>block(i))          //> Iteration: 1
                                                  //| Packet Length: 56 1
                                                  //| Confirmation Ready: true 1
                                                  //| Iteration: 2
                                                  //| Error message: Nice try! 1
                                                  //| Packet Length: 56 2
                                                  //| Confirmation Ready: true 2
                                                  //| Iteration: 3
                                                  //| Error message: Nice try! 2
                                                  //| Confirmation Ready: true 3
                                                  //| Error message: Oooops 3
                                                  //| Error message: Oooops 3
                                                  //| Iteration: 4
                                                  //| Packet Length: 65 4
                                                  //| Confirmation Ready: true 4
                                                  //| Iteration: 5
                                                  //| Received 4
                                                  //| Packet Length: 65 5
                                                  //| Confirmation Ready: true 5
                                                  //| Iteration: 6
                                                  //| Received 5
                                                  //| Error message: Oooops 6
                                                  //| Confirmation Ready: true 6
                                                  //| Iteration: 7
                                                  //| Error message: Oooops 6
                                                  //| Packet Length: 65 7
                                                  //| Confirmation Ready: true 7
                                                  //| Iteration: 8
                                                  //| Received 7
                                                  //| Packet Length: 83 8
                                                  //| Confirmation Ready: true 8
                                                  //| Iteration: 9
                                                  //| Error message: Guter Versuch! 8
                                                  //| Packet Length: 38 9
                                                  //| Confirmation Ready: true 9
                                                  //| Iteration: 10
                                                  //| Received 9
                                                  //| Packet Length: 65 10
                                                  //| Confirmation Ready: true 10
   //keeps the worksheet alive so the iterations can finish!
  blocking{Thread.sleep(3000)}                    //> Received 10-
  
}