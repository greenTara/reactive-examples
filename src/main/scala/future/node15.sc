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


object node15 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def flatMap[S, T](t: Future[T], f: T => Future[S])(implicit executor: ExecutionContext): Future[S] = async{
    //await {f(t)} //type mismatch for argument of f
    //f(t) //type mismatch for argument of f
    //f( await {t} ) //type mismatch for output, flattening required
    await { f( await {t} ) } //types ok, but does it work?
  }                                               //> flatMap: [S, T](t: scala.concurrent.Future[T], f: T => scala.concurrent.Futu
                                                  //| re[S])(implicit executor: scala.concurrent.ExecutionContext)scala.concurrent
                                                  //| .Future[S]

  
  
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
    
    def fun(i: Int): Future[Int] = Future[Int] {
       blocking{Thread.sleep(100*random.toInt)}
       if (fail)
         throw(new Error("Oooops!"))
       else i*i
    }
  
   val f2 = flatMap(f, fun )
   
    f2 onComplete {
      case Success(s) => println(s.toString  + " = (10 + " + i.toString + ")^2")
      case Failure(t:NoSuchElementException) => println(t.getMessage.toString  + " " + i.toString)
      case Failure(t) => println(t.getCause().toString  + " " + i.toString)
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
                                                  //| false
                                                  //| Iteration: 3
                                                  //| true
                                                  //| Iteration: 4
                                                  //| true
                                                  //| Iteration: 5
                                                  //| false
                                                  //| Iteration: 6
                                                  //| false
                                                  //| Iteration: 7
                                                  //| false
                                                  //| Iteration: 8
                                                  //| true
                                                  //| Iteration: 9
                                                  //| false
                                                  //| java.lang.Error: Oooops! 2
                                                  //| java.lang.Error: Oooops! 5
                                                  //| 100 = (10 + 0)^2
                                                  //| 361 = (10 + 9)^2
                                                  //| 289 = (10 + 7)^2
                                                  //| java.lang.Error: Oooops! 8
                                                  //| 121 = (10 + 1)^2
                                                  //| Iteration: 10
                                                  //| false
    blocking{Thread.sleep(3000)}                  //> java.lang.Error: Oooops! 3
                                                  //| 400 = (10 + 10)^2
                                                  //| java.lang.Error: Oooops! 4
                                                  //| java.lang.Error: Oooops! 6-
   
}