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

object ob21 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

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
  }                                               //> printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(indent: I
                                                  //| nt)(etime: () => Double)Unit


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
       
	}                                         //> block: (i: Int)(num: Int)Unit
  val gap = 15000                                 //> gap  : Int = 15000
	block(0)(-1)                              //> Observable: 0
                                                  //| 0 (  0.08 ) 3
                                                  //| 0 (  0.09 ) 2
                                                  //| 0 (  0.09 ) 1
                                                  //| 0 (  0.09 ) Completed
                                                  //| 0           (  0.21 ) rx.lang.scala.Observable$$anon$9@3904bcf6
                                                  //| 0           (  0.21 ) rx.lang.scala.Observable$$anon$9@6611d7b8
                                                  //| 0           (  0.21 ) rx.lang.scala.Observable$$anon$9@1d69a562
                                                  //| 0           (  0.21 ) Completed

  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> 0                     (  3.24 ) 3
                                                  //| 0                     (  6.23 ) 3
                                                  //| 0                     (  8.24 ) 2
                                                  //| 0                     ( 10.24 ) 2
                                                  //| 0                     ( 11.24 ) 1
                                                  //| 0                     ( 12.24 ) 1
                                                  //| 0                     ( 12.24 ) Completed
	block(1)(-1)                              //> Observable: 1
                                                  //| 1 (  0.02 ) 3
                                                  //| 1 (  0.02 ) 2
                                                  //| 1 (  0.02 ) 1
                                                  //| 1 (  0.02 ) Completed
                                                  //| 1           (  0.04 ) rx.lang.scala.Observable$$anon$9@173e4a85
                                                  //| 1           (  0.04 ) rx.lang.scala.Observable$$anon$9@7a8e6dae
                                                  //| 1           (  0.04 ) rx.lang.scala.Observable$$anon$9@1cc2f95e
                                                  //| 1           (  0.04 ) Completed
  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> 1                     (  1.07 ) 1
                                                  //| 1                     (  2.07 ) 1
                                                  //| 1                     (  2.07 ) 2
                                                  //| 1                     (  3.07 ) 3
                                                  //| 1                     (  4.07 ) 2
                                                  //| 1                     (  6.07 ) 3
                                                  //| 1                     (  6.07 ) Completed
  println("Done")                                 //> Done
   
}