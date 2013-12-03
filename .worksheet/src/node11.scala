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


object node11 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(489); 
  println("Welcome to the Scala worksheet");$skip(403); 
  
  /**
  * Retry successfully completing block at most noTimes
  * and give up after that
  */
  
   def retry[T](n: Int)(block: =>Future[T]): Future[T] = {
    val ns: Iterator[Int] = (1 to n).iterator
    val attempts: Iterator[()=>Future[T]] = ns.map(_ => ()=>block)
    val failed: Future[T] = Future.failed(new Exception)
    attempts.foldLeft(failed)((a, block) => a fallbackTo { block() })
  };System.out.println("""retry: [T](n: Int)(block: => scala.concurrent.Future[T])scala.concurrent.Future[T]""");$skip(114); 
  def rb(i: Int) = {
    blocking{Thread.sleep(100*random.toInt)}
    println("Hi " ++ i.toString)
    i + 10
  };System.out.println("""rb: (i: Int)Int""");$skip(357); 
  def block(i: Int) = {
    println("Iteration: " + i.toString)
    
    val ri = retry(i)( Future {rb(i)} )
    
    ri onComplete {
      case Success(s) => println(s.toString  ++ " = 10 + " ++ i.toString)
      case Failure(t:Exception) => println(t.toString  ++ " " ++ i.toString)
      case r => println(r.toString  ++ " " ++ i.toString)
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
