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
* In comparison, flatten merges the Observables, so they all run simultaneously
* (i.e. asynchronously).
*/

object ob21 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(682); 
  println("Welcome to the Scala worksheet");$skip(628); 

  def printOut[T](i:Int)(obs:Observable[T])(num:Int)(indent:Int)(etime: () => Double): Unit = {
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
  };System.out.println("""printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(indent: Int)(etime: () => Double)Unit""");$skip(659); 


  def block(i: Int)(num: Int) = {
    val t0 = System.nanoTime()
    def etime() = ((System.nanoTime() - t0).toDouble / 1e+9)
    println("Observable: " + i.toString)
    val xs: Observable[Int] = Observable(3,2,1)
    val yss: Observable[Observable[Int]] =
      xs.map(x => Observable.interval(x seconds).map(_=>x).take(2))
    val zs: Observable[Int] =
      if (i == 0) yss.concat
      else yss.flatten
    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOut(i)(xs)(num)(1)(etime)
    printOut(i)(yss)(num)(11)(etime)
    printOut(i)(zs)(num)(21)(etime)
       
	};System.out.println("""block: (i: Int)(num: Int)Unit""");$skip(18); 
  val gap = 15000;System.out.println("""gap  : Int = """ + $show(gap ));$skip(14); 
	block(0)(-1);$skip(69); 

  blocking{Thread.sleep(gap)};$skip(14);  // needed for asynchronous worksheets
	block(1)(-1);$skip(68); 
  blocking{Thread.sleep(gap)};$skip(18);  // needed for asynchronous worksheets
  println("Done")}
   
}
