package observable

import scala.language.postfixOps
import rx.lang.scala.{Observable, Subscription}
import scala.concurrent._
import duration._

/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Failure in Observables, as Marbles
*/

object ob14 {
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
 // There is a slight offset before each printing starts, to ensure things
 // come out in the right order.
 // This is pretty kludgy - if you don't like it, write a better one and
 // submit a pull request!
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


  def block(i: Int)(num: Int) = {
    println("Observable: " + i.toString)
    val ticks: Observable[Long] = Observable.interval(500 millis)
    val marbles: Observable[T] = ticks.take(6).map(i => (i, circles(i.toInt)._1, circles(i.toInt)._2) )
    val squareMarbles: Observable[T] = marbles.map( s => (s._1, s._2, "Square"))
    val fails: Observable[T] = marbles.take(3) ++ Observable(new Exception("My Bad")) ++ squareMarbles
    val eReturn: Observable[T] = fails.onErrorReturn( e => (-99, "Black", "Diamond" ) )
    val eResume: Observable[T] = fails.onErrorResumeNext(squareMarbles)
    // Unlike the iterable case, we are able to "traverse" the observable
    // multiple "times" through multiple subscriptions.
    printOutMarbles(i)(marbles)(num)(1)
    i match {
      case 0 => printOutMarbles(i)(fails)(num)(11)
      case 1 => printOutMarbles(i)(eReturn)(num)(21)
      case 2 => printOutMarbles(i)(eResume)(num)(31)
    }
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
                                                  //| 0 Yellow Circle
                                                  //| 0           Yellow Circle
                                                  //| 0 Green Circle
                                                  //| 0           Green Circle
                                                  //| 0           Ooops
                                                  //| 0 Aqua Circle
                                                  //| 0 Blue Circle
                                                  //| 0 Violet Circle
                                                  //| 0 Completed
	block(1)(-1)                              //> Observable: 1

  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> 1 Red Circle
                                                  //| 1                     Red Circle
                                                  //| 1 Yellow Circle
                                                  //| 1                     Yellow Circle
                                                  //| 1 Green Circle
                                                  //| 1                     Green Circle
                                                  //| 1                     Black Diamond
                                                  //| 1                     Completed
                                                  //| 1 Aqua Circle
                                                  //| 1 Blue Circle
                                                  //| 1 Violet Circle
                                                  //| 1 Completed
  block(2)(-1)                                    //> Observable: 2

  blocking{Thread.sleep(gap)} // needed for asynchronous worksheets
                                                  //> 2 Red Circle
                                                  //| 2                               Red Circle
                                                  //| 2 Yellow Circle
                                                  //| 2                               Yellow Circle
                                                  //| 2 Green Circle
                                                  //| 2                               Green Circle
                                                  //| 2 Aqua Circle
                                                  //| 2                               Red Square
                                                  //| 2 Blue Circle
                                                  //| 2                               Yellow Square
                                                  //| 2 Violet Circle
                                                  //| 2 Completed
                                                  //| 2                               Green Square
                                                  //| 2                               Aqua Square
                                                  //| 2                               Blue Square
                                                  //| 2                               Violet Square
                                                  //| 2                               Completed


  println("Done")                                 //> Done
   
}