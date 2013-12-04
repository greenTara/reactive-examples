package observable

import scala.language.postfixOps
import math.random
import rx.lang.scala.Observable
import scala.concurrent._
import scala.io.Source
import duration._
/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Basic methods of Observables
*/

object ob12 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(371); 
  println("Welcome to the Scala worksheet");$skip(255); 

 def printOut[T](i:Int)(obs:Observable[T])(num:Int)(msg:String): Unit = {
   val obsP =
     if (num > 0 ) obs.take(num)
     else obs
   obsP.subscribe( { it =>
      println("i = " + i.toString + ", obs:  " + msg + ", next = " + it.toString)
   })
  };System.out.println("""printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(msg: String)Unit""");$skip(522); 
     
  def block(i: Int)(num: Int) = {
    println("Observable: " + i.toString)
    
    val ticks: Observable[Long] = Observable.interval(1 second)
    val evens: Observable[Long] = ticks.filter(s => (s%2 == 0))
    val bufs: Observable[Seq[Long]] = ticks.buffer(2, 1)

    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOut(i)(ticks)(num)("ticks")
    printOut(i)(evens)(num)("evens")
    printOut(i)(bufs)(num)("bufs")
       
	};System.out.println("""block: (i: Int)(num: Int)Unit""");$skip(157); 

  // multiple executions of the block are not necessary for this demo
  // although they would be possible, and would execute asynchronously.
	block(0)(-1);$skip(240); 

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (10 seconds).
  blocking{Thread.sleep(10000)};$skip(18);  // needed for asynchronous worksheets
  println("Done")}
   
}
