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


object node14 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def filter[T](future: Future[T], predicate: T => Boolean)(implicit executor: ExecutionContext): Future[T] = async{
    val x: T = await{ future }
    if(!predicate(x)) {
      throw new NoSuchElementException("No such element")
    } else {
      x
    }
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
                                                  //| false
                                                  //| Iteration: 2
                                                  //| true
                                                  //| Iteration: 3
                                                  //| true
                                                  //| Iteration: 4
                                                  //| true
                                                  //| Iteration: 5
                                                  //| false
                                                  //| Iteration: 6
                                                  //| true
                                                  //| Iteration: 7
                                                  //| true
                                                  //| Iteration: 8
                                                  //| true
                                                  //| Iteration: 9
                                                  //| false
                                                  //| Iteration: 10
                                                  //| true
    blocking{Thread.sleep(3000)}                  //> 12 = 10 + 2
                                                  //| java.lang.Error: Oooops! 1
                                                  //| java.lang.Error: Oooops! 0
                                                  //| java.lang.Error: Oooops! 6
                                                  //| java.lang.Error: Oooops! 7
                                                  //| java.lang.Error: Oooops! 4
                                                  //| java.lang.Error: Oooops! 10
                                                  //| No such element 9
                                                  //| java.lang.Error: Oooops! 8
                                                  //| 13 = 10 + 3
                                                  //| No such element 5-
   
}