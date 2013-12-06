package observable

import scala.language.postfixOps
import rx.lang.scala.Observable
import scala.concurrent._
import duration._

/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Basic methods of Observables
*/

object ob12 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
    
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
  }                                               //> printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(msg: Strin
                                                  //| g)Unit
     
  def block(i: Int)(num: Int) = {
    println("Observable: " + i.toString)
    
    val ticks: Observable[Long] = Observable.interval((i*1000 + 500) millis)
    val evens: Observable[Long] = ticks.filter(s => (s%2 == 0))
    val bufs: Observable[Seq[Long]] = ticks.buffer(2, 1)
    val fails: Observable[Long] = ticks.take(3) ++ Observable(new Exception("oops")) ++ ticks
    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOut(i)(ticks)(num)("ticks")
    printOut(i)(evens)(num)("evens")
    printOut(i)(bufs)(num)("bufs")
    printOut(i)(fails)(num)("fails") // notice that this printOut stops after the failure
       
	}                                         //> block: (i: Int)(num: Int)Unit
  val gap = 5000                                  //> gap  : Int = 5000

  // multiple executions of the block execute asynchronously.
	(0 to 1 toList).foreach(i =>block(i)(-1)) //> Observable: 0
                                                  //| Observable: 1

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (5 seconds).
  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> i = 0, obs: ticks, next = 0
                                                  //| i = 0, obs: evens, next = 0
                                                  //| i = 0, obs: fails, next = 0
                                                  //| i = 0, obs: ticks, next = 1
                                                  //| i = 0, obs: bufs, next = Buffer(0, 1)
                                                  //| i = 0, obs: fails, next = 1
                                                  //| i = 0, obs: ticks, next = 2
                                                  //| i = 0, obs: evens, next = 2
                                                  //| i = 0, obs: bufs, next = Buffer(1, 2)
                                                  //| i = 0, obs: fails, next = 2
                                                  //| 0 Ooops
                                                  //| i = 1, obs: ticks, next = 0
                                                  //| i = 1, obs: evens, next = 0
                                                  //| i = 1, obs: fails, next = 0
                                                  //| i = 0, obs: ticks, next = 3
                                                  //| i = 0, obs: bufs, next = Buffer(2, 3)
                                                  //| i = 0, obs: ticks, next = 4
                                                  //| i = 0, obs: evens, next = 4
                                                  //| i = 0, obs: bufs, next = Buffer(3, 4)
                                                  //| i = 0, obs: ticks, next = 5
                                                  //| i = 0, obs: bufs, next = Buffer(4, 5)
                                                  //| i = 1, obs: ticks, next = 1
                                                  //| i = 1, obs: bufs, next = Buffer(0, 1)
                                                  //| i = 1, obs: fails, next = 1
                                                  //| i = 0, obs: ticks, next = 6
                                                  //| i = 0, obs: evens, next = 6
                                                  //| i = 0, obs: bufs, next = Buffer(5, 6)
                                                  //| i = 0, obs: ticks, next = 7
                                                  //| i = 0, obs: bufs, nex
                                                  //| Output exceeds cutoff limit.
  println("Done")                                 //> Done
   
}