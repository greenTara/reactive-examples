import math.random
import scala.language.postfixOps
import scala.util._
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, CanAwait, OnCompleteRunnable, TimeoutException, ExecutionException, blocking }
import scala.async.Async._
/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 5, "Promises ...".
*/


object node16 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def filter[T](future: Future[T], predicate: T => Boolean)(implicit executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    future.onComplete {
      case Success(s) => {
        if(!predicate(s)) {
          p.failure(new Exception("No such element"))
        } else {
          p.success(s)
        }
      }
      case Failure(f) => { p.failure(f) }
    }
    p.future
  }                                               //> filter: [T](future: scala.concurrent.Future[T], predicate: T => Boolean)(imp
                                                  //| licit executor: scala.concurrent.ExecutionContext)scala.concurrent.Future[T]
                                                  //| 

  
  
  def block(i: Int) = {
    println("Iteration: " + i.toString)
 
    def fail = (random < 0.5)
    println(fail.toString)
    val f = Future[Int] {
       blocking{Thread.sleep(100*random.toInt)}
       if (fail)
         throw(new Error("Oooops!"))
       else i + 10
    }
  
   val f2 = filter[Int](f, (i => (i < 15)))
   
    f2 onComplete {
      case Success(s) => println(s.toString  ++ " = 10 + " ++ i.toString)
      case Failure(t:NoSuchElementException) => println(t.getMessage.toString  ++ " " ++ i.toString)
      case Failure(t) => println(t.getCause().toString  ++ " " ++ i.toString)
      }
    
	}                                         //> block: (i: Int)Unit
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 10 toList).foreach(i =>block(i))          //> Iteration: 0
                                                  //| true
                                                  //| Iteration: 1
                                                  //| true
                                                  //| Iteration: 2
                                                  //| false
                                                  //| Iteration: 3
                                                  //| true
                                                  //| Iteration: 4
                                                  //| false
                                                  //| Iteration: 5
                                                  //| true
                                                  //| Iteration: 6
                                                  //| false
                                                  //| Iteration: 7
                                                  //| false
                                                  //| Iteration: 8
                                                  //| false
                                                  //| Iteration: 9
                                                  //| false
                                                  //| Iteration: 10
                                                  //| false
   
    blocking{Thread.sleep(3000)}                  //> java.lang.NullPointerException
                                                  //| 	at node16$$anonfun$main$1$$anonfun$node16$$anonfun$$block$1$1.apply(node
                                                  //| 16.scala:52)
                                                  //| 	at node16$$anonfun$main$1$$anonfun$node16$$anonfun$$block$1$1.apply(node
                                                  //| 16.scala:49)
                                                  //| 	at scala.concurrent.impl.CallbackRunnable.run(Promise.scala:29)
                                                  //| 	at scala.concurrent.impl.ExecutionContextImpl$$anon$3.exec(ExecutionCont
                                                  //| extImpl.scala:107)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinTask.doExec(ForkJoinTask.java:260)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinPool$WorkQueue.pollAndExecAll(ForkJ
                                                  //| oinPool.java:1253)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinPool$WorkQueue.runTask(ForkJoinPool
                                                  //| .java:1346)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinPool.runWorker(ForkJoinPool.java:19
                                                  //| 79)
                                                  //| 	at scala.concurrent.forkjoin.ForkJoinWorkerThread.run(ForkJoinWorkerThre
                                                  //| ad.java:107)
                                                  //| java.lang.Error: Oooops! 6
                                                  //| java.lang.Error: Oooops! 7
                                                  //| 10 = 10 + 0
                                                  //| java.lang.Error: Oooops! 4
                                                  //| 11 = 10 + 1
                                                  //| 
                                                  //| Output exceeds cutoff limit.-
   
}