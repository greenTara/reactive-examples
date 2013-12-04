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

object ob12 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

 def printOut[T](i:Int)(obs:Observable[T])(num:Int)(msg:String): Unit = {
   val obsP =
     if (num > 0 ) obs.take(num)
     else obs
   obsP.subscribe( { it =>
      println("i = " + i.toString + ", obs:  " + msg + ", next = " + it.toString)
   })
  }                                               //> printOut: [T](i: Int)(obs: rx.lang.scala.Observable[T])(num: Int)(msg: Strin
                                                  //| g)Unit
     
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
       
	}                                         //> block: (i: Int)(num: Int)Unit

  // multiple executions of the block are not necessary for this demo
  // although they would be possible, and would execute asynchronously.
	block(0)(-1)                              //> Observable: 0

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (10 seconds).
  blocking{Thread.sleep(10000)} // needed for asynchronous worksheets
                                                  //> i = 0, obs:  ticks, next = 0
                                                  //| i = 0, obs:  evens, next = 0
                                                  //| i = 0, obs:  ticks, next = 1
                                                  //| i = 0, obs:  bufs, next = Buffer(0, 1)
                                                  //| i = 0, obs:  ticks, next = 2
                                                  //| i = 0, obs:  evens, next = 2
                                                  //| i = 0, obs:  bufs, next = Buffer(1, 2)
                                                  //| i = 0, obs:  ticks, next = 3
                                                  //| i = 0, obs:  bufs, next = Buffer(2, 3)
                                                  //| i = 0, obs:  ticks, next = 4
                                                  //| i = 0, obs:  evens, next = 4
                                                  //| i = 0, obs:  bufs, next = Buffer(3, 4)
                                                  //| i = 0, obs:  ticks, next = 5
                                                  //| i = 0, obs:  bufs, next = Buffer(4, 5)
                                                  //| i = 0, obs:  ticks, next = 6
                                                  //| i = 0, obs:  evens, next = 6
                                                  //| i = 0, obs:  bufs, next = Buffer(5, 6)
                                                  //| i = 0, obs:  ticks, next = 7
                                                  //| i = 0, obs:  bufs, next = Buffer(6, 7)
                                                  //| i = 0, obs:  ticks, next = 8
                                                  //| i = 0, obs:  evens, next = 8
                                                  //| i = 0, obs:  bufs, next = Buffer(7, 8)
                                                  //| i = 0, obs:  ticks, next = 9
                                                  //| i = 0, obs:  bufs, next = Buffer(8, 9)
  println("Done")                                 //> Done
   
}