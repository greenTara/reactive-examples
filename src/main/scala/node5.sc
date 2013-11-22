package nodescala

import math.random
import scala.language.postfixOps
import scala.util._
import control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global


object node5 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

    val EMail1 = (for {i <- 0 to 11} yield (random*256).toByte).toArray
                                                  //> EMail1  : Array[Byte] = Array(67, 75, 32, 22, -89, -13, 73, -24, 124, 41, -9
                                                  //| , -42)
    val EMail2 = (for {i <- 0 to 10} yield (random*256).toByte).toArray
                                                  //> EMail2  : Array[Byte] = Array(-53, -112, 55, -102, -23, 26, -81, 64, -61, 79
                                                  //| , -28)
    
  abstract class Confirmation{
    val max: Int
    }
  
  trait Socket {
    def readFromMemory(): Future[Array[Byte]]
    def sentToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }
 
  def disconnect(a:Socket) = (random < 0.5)       //> disconnect: (a: nodescala.node5.Socket)Boolean
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
      blocking {Thread.sleep(1000)}
      EMail2
    }
    else {
      blocking {Thread.sleep(100)}
      EMail1
    }                                             //> packetSource: (rand: Double, prob: Double)Array[Byte]
  
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
  }

  val socket = Socket()                           //> socket  : nodescala.node5.Socket = nodescala.node5$$anonfun$main$1$Socket$3
                                                  //| $$anon$1@70984b95
 val confirmation: Future[Array[Byte]] = for {
   packet <- socket.readFromMemory()
   confirmation <- socket.sentToEurope(packet)
 } yield confirmation                             //> confirmation  : scala.concurrent.Future[Array[Byte]] = scala.concurrent.imp
                                                  //| l.Promise$DefaultPromise@379bc1bf
 println(Await.result(confirmation, 10 second))   //> java.util.concurrent.ExecutionException: Boxed Error
                                                  //| 	at scala.concurrent.impl.Promise$.resolver(Promise.scala:52)
                                                  //| 	at scala.concurrent.impl.Promise$.scala$concurrent$impl$Promise$$resolve
                                                  //| Try(Promise.scala:44)
                                                  //| 	at scala.concurrent.impl.Promise$DefaultPromise.tryComplete(Promise.scal
                                                  //| a:116)
                                                  //| 	at scala.concurrent.Promise$class.complete(Promise.scala:55)
                                                  //| 	at scala.concurrent.impl.Promise$DefaultPromise.complete(Promise.scala:5
                                                  //| 8)
                                                  //| 	at scala.concurrent.impl.Future$PromiseCompletingRunnable.run(Future.sca
                                                  //| la:23)
                                                  //| 	at scala.concurrent.impl.ExecutionContextImpl$$anon$3.exec(ExecutionCont
                                                  //| extImpl.scala:107)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinTask.doExec(ForkJoinTask.java:260)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinPool$WorkQueue.runTask(ForkJoinPool
                                                  //| .java:1339)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinPool.runWorker(ForkJoinPool.java:19
                                                  //| 79)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinWorkerThread.run(ForkJoin
                                                  //| Output exceeds cutoff limit.
   
}