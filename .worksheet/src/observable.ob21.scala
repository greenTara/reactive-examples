package observable

import scala.language.postfixOps
import rx.lang.scala.{Observable, Subscription}
import scala.concurrent._
import duration._
import java.util.Calendar

/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 2, "Basic Combinators on Observables".
* How concat really works -
* The later observables (in the concat sequence) aren't subscribed to until
* the earlier observables are finished.
* So, there are additional delays, as can be seen by the elapsed time in seconds.
*/

object ob21 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(577); 
  println("Welcome to the Scala worksheet");$skip(30); 

  val t0 = System.nanoTime();System.out.println("""t0  : Long = """ + $show(t0 ));$skip(59); 
  def etime() = ((System.nanoTime() - t0).toDouble / 1e+9);System.out.println("""etime: ()Double""");$skip(606); 
  def printOut[T](i:Int)(obs:Observable[T])(num:Int)(indent:Int): Unit = {
    blocking{Thread.sleep(20)}
    val is:String = i.toString.padTo(indent, ' ' )
    val obsP =
      if (num > 0 ) obs.take(num)
      else obs
    obsP.subscribe(
      it => {
        val now = etime()
        val itOut = it.toString
                 println(f"$is ( $now%5.2f ) $itOut")
      },
        error => {
        val now = etime()
                 println(f"$is ( $now%5.2f ) Ooops")
        },
        () =>    {
        val now = etime()
                 println(f"$is ( $now%5.2f ) Completed")
        }
   )
  };System.out.println("""printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(indent: Int)Unit""");$skip(505); 


  def block(i: Int)(num: Int) = {
    println("Observable: " + i.toString)
    val xs: Observable[Int] = Observable(3,2,1)
    val yss: Observable[Observable[Int]] =
      xs.map(x => Observable.interval(x seconds).map(_=>x).take(2))
    val zs: Observable[Int] = yss.concat
    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOut(i)(xs)(num)(1)
    printOut(i)(yss)(num)(11)
    printOut(i)(zs)(num)(21)
       
	};System.out.println("""block: (i: Int)(num: Int)Unit""");$skip(18); 
  val gap = 15000;System.out.println("""gap  : Int = """ + $show(gap ));$skip(156); 
  // multiple executions of the block are not necessary for this demo
  // although they would be possible, and would execute asynchronously.
	block(0)(-1);$skip(237); 

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (5 seconds).
  blocking{Thread.sleep(gap)};$skip(18);  // needed for asynchronous worksheets
  println("Done")}
   
}
