import math.random
import scala.language.postfixOps
import scala.util._
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, CanAwait, OnCompleteRunnable, TimeoutException, ExecutionException, blocking }
/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 4, "Composing Futures".
*/


object node11 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  /**
  * Retry successfully completing block at most noTimes
  * and give up after that
  */
  def retry[T](noTimes: Int)(block: =>Future[T]): Future[T] = {
    if (noTimes ==0) {
     Future.failed(new Exception("Sorry"))
    } else {
      block fallbackTo {
        retry(noTimes - 1) { block }
      }
    }
  }                                               //> retry: [T](noTimes: Int)(block: => scala.concurrent.Future[T])scala.concurre
                                                  //| nt.Future[T]
  def rb(i: Int) = {
    println("Hi " ++ i.toString)
    i + 10
  }                                               //> rb: (i: Int)Int
  def block(i: Int) = {
    println("Iteration: " + i.toString)
    
    val ri = retry(i)( Future {rb(i)} )
    
    ri onComplete {
      case r => println(r.toString  + " " + i.toString)
    }
    
	}                                         //> block: (i: Int)Unit
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 4 toList).foreach(i =>block(i))           //> Iteration: 0
                                                  //| Iteration: 1
                                                  //| Failure(java.lang.Exception: Sorry) 0
                                                  //| Iteration: 2
                                                  //| Iteration: 3
                                                  //| Iteration: 4
    blocking{Thread.sleep(3000)}                  //> Hi 3
                                                  //| Hi 3
                                                  //| Hi 4
                                                  //| Hi 4
                                                  //| Hi 1
                                                  //| Hi 2
                                                  //| Hi 2
                                                  //| Hi 3
                                                  //| Hi 4
                                                  //| Success(11) 1
                                                  //| Success(14) 4
                                                  //| Success(13) 3
                                                  //| Success(12) 2
                                                  //| Hi 4-


   
}