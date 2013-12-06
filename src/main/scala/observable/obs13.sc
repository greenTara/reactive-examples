package observable

import scala.language.postfixOps
import rx.lang.scala.{Observable, Subscription}
import scala.concurrent._
import duration._

/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Basic methods of Observables, as Marbles
*/

object ob13 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  type P = (String, String)
  type T = (Long, String, String)
  type S = Seq[T]
  
  val circles: Array[P] = Array(
     ("Red", "Circle"),
     ("Yellow", "Circle"),
     ("Green", "Circle"),
     ("Aqua", "Circle"),
     ("Blue", "Circle"),
     ("Violet", "Circle")
  )                                               //> circles  : Array[(String, String)] = Array((Red,Circle), (Yellow,Circle), (G
                                                  //| reen,Circle), (Aqua,Circle), (Blue,Circle), (Violet,Circle))

 // This formatted printout is not as pretty as marble diagrams,
 // but attempts to capture the same content given what we have availabe
 // in the worksheet mode.
 // We use variable indentation to indicate the output of different observables.
 // There is a slight time offset before each printing starts, to ensure things
 // come out in the right order.
 def printOutMarbles(i:Int)(obs:Observable[T])(num:Int)(indent:Int): Unit = {
   blocking{Thread.sleep(20)}
   val is:String = i.toString.padTo(indent, ' ' )
    
   val obsP =
     if (num > 0 ) obs.take(num)
     else obs
  obsP.subscribe (
			   it => {
   			     val color = it._2
		  	     val shape = it._3
			           println(f"$is $color $shape")
			     } ,
        error => println(f"$is Ooops"),
        () =>    println(f"$is Completed")
        )
    }                                             //> printOutMarbles: (i: Int)(obs: rx.lang.scala.Observable[(Long, String, Stri
                                                  //| ng)])(num: Int)(indent: Int)Unit


def printOutBufMarbles(i:Int)(obs:Observable[S])(num:Int)(indent:Int): Unit = {
   val is = i.toString.padTo(indent, ' ')
   blocking{Thread.sleep(20)}
   val obsP =
     if (num > 0 ) obs.take(num)
     else obs
	    obsP.subscribe(
	      it => {
   			     val marble0 = it(0)
   			     val marble1 = it(1)
		  	     val color0 = marble0._2
		  	     val color1 = marble1._2
		  	     val shape0 = marble0._3
		  	     val shape1 = marble1._3
			           println(f"$is ($color0 $shape0, $color1 $shape1)" )
			     } ,
        error => println(f"$is Ooops"),
        () =>    println(f"$is Completed")
        )
  }                                               //> printOutBufMarbles: (i: Int)(obs: rx.lang.scala.Observable[observable.ob13.
                                                  //| S])(num: Int)(indent: Int)Unit

  def block(i: Int)(num: Int) = {
    println("Observable: " + i.toString)
    val ticks: Observable[Long] = Observable.interval(500 millis)
    val marbles: Observable[T] = ticks.take(6)map(i => (i, circles(i.toInt)._1, circles(i.toInt)._2) )
    val evenMarbles: Observable[T] = marbles.filter( s => (s._1 % 2 == 0))
    val squareMarbles: Observable[T] = marbles.map( s => (s._1, s._2, "Square"))
    val bufMarbles: Observable[Seq[T]] = marbles.buffer(2, 1)
    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOutMarbles(i)(marbles)(num)(1)
    printOutMarbles(i)(evenMarbles)(num)(11)
    printOutMarbles(i)(squareMarbles)(num)(21)
    printOutBufMarbles(i)(bufMarbles)(num)(31)
       
	}                                         //> block: (i: Int)(num: Int)Unit
  val gap = 5000                                  //> gap  : Int = 5000
 
  // multiple executions of the block are not necessary for this demo
  // although they would be possible, and would execute asynchronously.
	block(0)(-1)                              //> Observable: 0

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (10 seconds).
  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> 0 Red Circle
                                                  //| 0           Red Circle
                                                  //| 0                     Red Square
                                                  //| 0 Yellow Circle
                                                  //| 0                     Yellow Square
                                                  //| 0                               (Red Circle, Yellow Circle)
                                                  //| 0 Green Circle
                                                  //| 0           Green Circle
                                                  //| 0                     Green Square
                                                  //| 0                               (Yellow Circle, Green Circle)
                                                  //| 0 Aqua Circle
                                                  //| 0                     Aqua Square
                                                  //| 0                               (Green Circle, Aqua Circle)
                                                  //| 0 Blue Circle
                                                  //| 0           Blue Circle
                                                  //| 0                     Blue Square
                                                  //| 0                               (Aqua Circle, Blue Circle)
                                                  //| 0 Violet Circle
                                                  //| 0 Completed
                                                  //| 0           Completed
                                                  //| 0                     Violet Square
                                                  //| 0                     Completed
                                                  //| 0                               (Blue Circle, Violet Circle)
                                                  //| 0                               Ooops
  println("Done")                                 //> Done
   
}