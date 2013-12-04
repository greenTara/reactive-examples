package observable

import math.random
import rx.lang.scala.Observable
import scala.concurrent._
import scala.io.Source
/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Review of Iterators
*/



object ob11 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(313); 
  println("Welcome to the Scala worksheet");$skip(153); 

 def printOut[T](i:Int)(iter:Iterator[T]): Unit =
   while(iter.hasNext) {
      println("i = " + i.toString + ", next = " + iter.next().toString)
   };System.out.println("""printOut: [T](i: Int)(iter: Iterator[T])Unit""");$skip(105); 
   
  def ReadLinesFromDisk(path: String): Iterator[String] = {
    Source.fromFile(path).getLines()
  };System.out.println("""ReadLinesFromDisk: (path: String)Iterator[String]""");$skip(51); 
   
  val url = getClass().getResource("poem.txt");System.out.println("""url  : java.net.URL = """ + $show(url ));$skip(27); 
  val path = url.getPath();System.out.println("""path  : String = """ + $show(path ));$skip(613); 
   
  def block(i: Int) = {
    println("Iteration: " + i.toString)
    
    val iter0 = (0 to (i+2) by 2).iterator
    val iter1 = (1 to (i+3) by 2).iterator
   
    val iter = i match {
      case 0  =>  iter0.flatMap( n => (n*10 to (n*10 + 5) by 1).iterator )
      case 1 => iter0.map( n => n*n )
      case 2 => iter0 ++ iter1
      case 3 => iter0.filter( n => n%4 == 0 )
      case 4 => iter0.take(3)
      case 5 => iter0.takeWhile(n => (n < 5))
      case 6 => iter0.zip(iter1)
      case 7 => ReadLinesFromDisk(path)
   }
   printOut(i)(iter) //An iterator can't be traversed more than once.
       
	};System.out.println("""block: (i: Int)Unit""");$skip(354); 
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 7 toList).foreach(i =>block(i))}
   
    //blocking{Thread.sleep(3000)} // only needed for asynchronous worksheets
   
}
