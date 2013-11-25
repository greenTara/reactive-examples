import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global

/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 3, "Combinators on Futures", especially Slide 8.
* The sending commands are made more complicated, so that failure
* can occur in multiple ways. In particular, a sending to Europe
* can fail, or a sending to the USA, or both.
* Using recoverWith and recover, there are fewer failures.
*/
object node8 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val EMail1 = (for {i <- 0 to 1} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(-103, -28)
  val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(-6, -124, -61, -66, 1, -9, 82, -71, -47, 127, 
                                                  //| -25)
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
               usaError.getCause().getMessage.map(x => x.toByte).toArray
             //case usaError =>
             //  usaError.getMessage.map(x => x.toByte).toArray
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
        socket.sendToSafe( p )
        })
    Await.ready(confirmation, 1 second)
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
  (1 to 10 toList).foreach(i =>block(i))          //> Iteration: 1
                                                  //| Error message: Oooops 1
                                                  //| Confirmation Ready: true 1
                                                  //| Iteration: 2
                                                  //| Error message: Oooops 1
                                                  //| Packet Length: 56 2
                                                  //| Confirmation Ready: true 2
                                                  //| Iteration: 3
                                                  //| scala.MatchError: Failure(java.lang.NullPointerException) (of class scala.u
                                                  //| til.Failure)
                                                  //| 	at node8$$anonfun$main$1$$anonfun$node8$$anonfun$$block$1$2.apply(node8.
                                                  //| scala:141)
                                                  //| 	at node8$$anonfun$main$1$$anonfun$node8$$anonfun$$block$1$2.apply(node8.
                                                  //| scala:141)
                                                  //| 	at scala.concurrent.impl.CallbackRunnable.run(Promise.scala:29)
                                                  //| 	at scala.concurrent.impl.ExecutionContextImpl$$anon$3.exec(ExecutionCont
                                                  //| extImpl.scala:107)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinTask.doExec(ForkJoinTask.java:260)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinPool$WorkQueue.runTask(ForkJoinPool
                                                  //| .java:1339)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinPool.runWorker(ForkJoinPool.java:19
                                                  //| 79)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinWorkerThread.run(ForkJoinWorkerT
                                                  //| Output exceeds cutoff limit.
   //keeps the worksheet alive so the iterations can finish!
  blocking{Thread.sleep(3000)}                    //> Message: Received 10-
  
}