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


object node10 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(489); 
  println("Welcome to the Scala worksheet");$skip(320); 
  
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
  };System.out.println("""retry: [T](noTimes: Int)(block: => scala.concurrent.Future[T])scala.concurrent.Future[T]""");$skip(69); 
  def rb(i: Int) = {
    println("Hi " ++ i.toString)
    i + 10
  };System.out.println("""rb: (i: Int)Int""");$skip(204); 
  def block(i: Int) = {
    println("Iteration: " + i.toString)
    
    val ri = retry(i)( Future {rb(i)} )
    
    ri onComplete {
      case r => println(r.toString  + " " + i.toString)
    }
    
	};System.out.println("""block: (i: Int)Unit""");$skip(354); 
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 4 toList).foreach(i =>block(i));$skip(33); 
    blocking{Thread.sleep(3000)}}


   
}
