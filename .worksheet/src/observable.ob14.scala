package observable

import scala.language.postfixOps
import rx.lang.scala.{Observable, Subscription}
import scala.concurrent._
import duration._

/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Failure in Observables, as Marbles
*/

object ob14 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(352); 
  println("Welcome to the Scala worksheet")
  
  type P = (String, String)
  type T = (Long, String, String)
  type S = Seq[T];$skip(276); 
  
  val circles: Array[P] = Array(
     ("Red", "Circle"),
     ("Yellow", "Circle"),
     ("Green", "Circle"),
     ("Aqua", "Circle"),
     ("Blue", "Circle"),
     ("Violet", "Circle")
  );System.out.println("""circles  : Array[(String, String)] = """ + $show(circles ));$skip(924); 

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
    };System.out.println("""printOutMarbles: (i: Int)(obs: rx.lang.scala.Observable[(Long, String, String)])(num: Int)(indent: Int)Unit""");$skip(941); 


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
	};System.out.println("""block: (i: Int)(num: Int)Unit""");$skip(17); 
  val gap = 5000;System.out.println("""gap  : Int = """ + $show(gap ));$skip(156); 
  // multiple executions of the block are not necessary for this demo
  // although they would be possible, and would execute asynchronously.
	block(0)(-1);$skip(238); 

  // We are printing out observables of infinite length, so
  // the only reason the worksheet terminates is that we block here
  // for a finite duration (10 seconds).
  blocking{Thread.sleep(gap)};$skip(14);  // needed for asynchronous worksheets
	block(1)(-1);$skip(69); 

  blocking{Thread.sleep(gap)};$skip(15);  // needed for asynchronous worksheets
  block(2)(-1);$skip(69); 

  blocking{Thread.sleep(gap)};$skip(20);  // needed for asynchronous worksheets


  println("Done")}
   
}
