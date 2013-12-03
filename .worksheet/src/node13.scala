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
* Week3, Lecture 4, "Composing Futures".
*/


object node13 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(516); 
  println("Welcome to the Scala worksheet");$skip(266); 
  
  /**
  * Retry successfully completing block at most noTimes
  * and give up after that
  */

  def withTry[T](future: Future[T])(implicit executor: ExecutionContext): Future[Try[T]] = {
    future.map(Success(_)) recover { case t: Throwable => Failure(t) }
  };System.out.println("""withTry: [T](future: scala.concurrent.Future[T])(implicit executor: scala.concurrent.ExecutionContext)scala.concurrent.Future[scala.util.Try[T]]""");$skip(373); 
  
 def retry[T](n: Int)(block: =>Future[T])(implicit executor: ExecutionContext): Future[T] = async {
    var i: Int = 0
    var result: Try[T] = Failure(new Exception("Oops"))

    while (i < n) {
      result = await { withTry(block) }

      result match {
        case Success(s) => { i = i + 1 }
        case Failure(f) => { i = n }
      }
    }

    result.get
  };System.out.println("""retry: [T](n: Int)(block: => scala.concurrent.Future[T])(implicit executor: scala.concurrent.ExecutionContext)scala.concurrent.Future[T]""");$skip(130); 


             
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
