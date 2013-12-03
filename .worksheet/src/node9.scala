import math.random
import scala.language.postfixOps
import scala.util._
 /* We define our own Try, Success and Failure in order to experiment
* with the alternate constructor proposed in the Quiz.
*/
//import scala.util.{Try, Success, Failure}
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, CanAwait, OnCompleteRunnable, TimeoutException, ExecutionException, blocking }
/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 3, "Combinators on Futures", particularly the Quiz.
* The requirements are to implement a constructor for Try that
* takes a Future[T] and "materializes" the exception. That means
* if the asynchronous computation succeeds, we wrap the result in a Success,
* if it fails we wrap it in a Failure.
* In either case, the Future[Try[T]] is considered successful.
*/


object node9 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(953); 
  println("Welcome to the Scala worksheet")
  
  abstract class Try[+T] {
      def flatMap[S](f: T=>Try[S]): Try[S] = this match {
		    case Success(value)   => try {f(value) } catch { case t:Throwable => Failure(t)} //(c)
		    case failure @ Failure(t)        => Failure(t) //fixed typo
		  }
  }
  
  case class Success[+T](elem: T) extends Try[T]
  
  case class Failure(t: Throwable) extends Try[Nothing]
  
  object Try {
    def apply[T](r: =>T): Try[T] = {

		  
      try { Success(r) }
      catch { case t:Throwable => Failure(t) }
  
    }
    def apply[T](f: Future[T]): Future[Try[T]] = {
      //f onComplete { x => x} // type mismatch
      //f recoverWith { case t => Future.failed(t) } //type mismatch
      //f.map(x => Try(x))  // in case Try(x) fails, returns Failure, not Success(Failure)
     f.map(s=>Success(s)) recover { case t => Failure(t)}
    }
  };$skip(1285); 
  
  
  /* Exception handling with flatMap.
  */
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
    val ftry:Future[Try[Int]] = Try( f )
    ftry onComplete {case r => println(r.toString  ++ " " ++ i.toString)  }
    
	};System.out.println("""block: (i: Int)Unit""");$skip(355); 
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (1 to 10 toList).foreach(i =>block(i));$skip(33); 
    blocking{Thread.sleep(3000)}}


   
}
