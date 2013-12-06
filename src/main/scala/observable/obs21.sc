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

object ob21 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val t0 = System.nanoTime()                      //> t0  : Long = 1386371103008956000
  def etime() = ((System.nanoTime() - t0).toDouble / 1e+9)
                                                  //> etime: ()Double
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
  }                                               //> printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(indent: I
                                                  //| nt)Unit


  def block(i: Int)(num: Int) = {
    println("Observable: " + i.toString)
    val xs: Observable[Int] = Observable(3,2,1)
    val yss: Observable[Observable[Int]] =
      xs.map(x => Observable.interval(x seconds).map(_=>x).take(2))
    val zs: Observable[Int] =
      if (i == 0) yss.concat
      else yss.flatten
    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOut(i)(xs)(num)(1)
    printOut(i)(yss)(num)(11)
    printOut(i)(zs)(num)(21)
       
	}                                         //> block: (i: Int)(num: Int)Unit
  val gap = 15000                                 //> gap  : Int = 15000
  // multiple executions of the block are not necessary for this demo
  // although they would be possible, and would execute asynchronously.
	block(0)(-1)                              //> Observable: 0
                                                  //| 0 (  0.08 ) 3
                                                  //| 0 (  0.09 ) 2
                                                  //| 0 (  0.09 ) 1
                                                  //| 0 (  0.09 ) Completed
                                                  //| 0           (  0.20 ) rx.lang.scala.Observable$$anon$9@1d69a562
                                                  //| 0           (  0.20 ) rx.lang.scala.Observable$$anon$9@2e8d404
                                                  //| 0           (  0.20 ) rx.lang.scala.Observable$$anon$9@3b7541a
                                                  //| 0           (  0.20 ) Completed

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (5 seconds).
  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> 0                     (  3.23 ) 3
                                                  //| 0                     (  6.23 ) 3
                                                  //| 0                     (  8.23 ) 2
                                                  //| 0                     ( 10.23 ) 2
                                                  //| 0                     ( 11.23 ) 1
                                                  //| 0                     ( 12.23 ) 1
                                                  //| 0                     ( 12.23 ) Completed
	block(1)(-1)                              //> Observable: 1
                                                  //| 1 ( 15.26 ) 3
                                                  //| 1 ( 15.26 ) 2
                                                  //| 1 ( 15.26 ) 1
                                                  //| 1 ( 15.26 ) Completed
                                                  //| 1           ( 15.28 ) rx.lang.scala.Observable$$anon$9@7a8e6dae
                                                  //| 1           ( 15.28 ) rx.lang.scala.Observable$$anon$9@1cc2f95e
                                                  //| 1           ( 15.28 ) rx.lang.scala.Observable$$anon$9@44819912
                                                  //| 1           ( 15.28 ) Completed
  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> 1                     ( 16.31 ) 1
                                                  //| 1                     ( 17.31 ) 2
                                                  //| 1                     ( 17.31 ) 1
                                                  //| 1                     ( 18.31 ) 3
                                                  //| 1                     ( 19.31 ) 2
                                                  //| 1                     ( 21.31 ) 3
                                                  //| 1                     ( 21.33 ) Completed
  println("Done")                                 //> Done
   
}