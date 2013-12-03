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


object node14 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(516); 
  println("Welcome to the Scala worksheet");$skip(264); 
  
  def filter[T](future: Future[T], predicate: T => Boolean)(implicit executor: ExecutionContext): Future[T] = async{
    val x: T = await{ future }
    if(!predicate(x)) {
      throw new NoSuchElementException("No such element")
    } else {
      x
    }
  };System.out.println("""filter: [T](future: scala.concurrent.Future[T], predicate: T => Boolean)(implicit executor: scala.concurrent.ExecutionContext)scala.concurrent.Future[T]""");$skip(623); 

  
  
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
    
	};System.out.println("""block: (i: Int)Unit""");$skip(355); 
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 10 toList).foreach(i =>block(i));$skip(33); 
    blocking{Thread.sleep(3000)}}
   
}
