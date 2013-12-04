package observable

import scala.language.postfixOps
import rx.lang.scala.Observable
import scala.concurrent._
import duration._

/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Basic methods of Observables
*/

object ob12 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(330); 
  println("Welcome to the Scala worksheet");$skip(408); 
    
 def printOut[T](i:Int)(obs:Observable[T])(num:Int)(msg:String): Unit = {
   blocking{Thread.sleep(20)}
   val is = i.toString
   val obsP =
     if (num > 0 ) obs.take(num)
     else obs
   obsP.subscribe(
     it => {
      val itOut = it.toString
      println(f"i = $is, obs: $msg, next = $itOut")
      },
        error => println(f"$is Ooops"),
        () =>    println(f"$is Completed")
   )
  };System.out.println("""printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(msg: String)Unit""");$skip(711); 
     
  def block(i: Int)(num: Int) = {
    println("Observable: " + i.toString)
    
    val ticks: Observable[Long] = Observable.interval((i + 1) second)
    val evens: Observable[Long] = ticks.filter(s => (s%2 == 0))
    val bufs: Observable[Seq[Long]] = ticks.buffer(2, 1)
    val fails: Observable[Long] = ticks.take(3) ++ Observable(new Exception("oops")) ++ ticks
    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOut(i)(ticks)(num)("ticks")
    printOut(i)(evens)(num)("evens")
    printOut(i)(bufs)(num)("bufs")
    printOut(i)(fails)(num)("fails") // notice that this printOut stops after the failure
       
	};System.out.println("""block: (i: Int)(num: Int)Unit""");$skip(106); 

  // multiple executions of the block execute asynchronously.
	(0 to 1 toList).foreach(i =>block(i)(-1));$skip(238); 

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (5 seconds).
  blocking{Thread.sleep(5000)};$skip(18);  // needed for asynchronous worksheets
  println("Done")}
   
}
