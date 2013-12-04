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
          p.failure(new NoSuchElementException("No such element"))
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
  
   val f2 = filter[Int](f, (i => (i < 12)))
   
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
  (0 to 4 toList).foreach(i =>block(i))           //> Iteration: 0
                                                  //| false
                                                  //| Iteration: 1
                                                  //| true
                                                  //| Iteration: 2
                                                  //| true
                                                  //| Iteration: 3
                                                  //| true
                                                  //| Iteration: 4
                                                  //| true
   
    blocking{Thread.sleep(3000)}                  //> 11 = 10 + 1
                                                  //| No such element 2
                                                  //| java.lang.Error: Oooops! 0
                                                  //| java.lang.Error: Oooops! 3
                                                  //| No such element 4-
   
}