package observable

import math.random
import rx.lang.scala.Observable
import scala.concurrent._
import scala.io.Source
/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Review of Iterators
*/

object ob11 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(311); 
  println("Welcome to the Scala worksheet");$skip(234); 

 def printOut[T](i:Int)(iter:Iterator[T])(num:Int): Unit = {
   val iterP =
     if (num > 0 ) iter.take(num)
     else iter
   while(iterP.hasNext) {
      println("i = " + i.toString + ", next = " + iterP.next().toString)
   }
  };System.out.println("""printOut: [T](i: Int)(iter: Iterator[T])(num: Int)Unit""");$skip(105); 
   
  def ReadLinesFromDisk(path: String): Iterator[String] = {
    Source.fromFile(path).getLines()
  };System.out.println("""ReadLinesFromDisk: (path: String)Iterator[String]""");$skip(51); 
   
  val url = getClass().getResource("poem.txt");System.out.println("""url  : java.net.URL = """ + $show(url ));$skip(27); 
  val path = url.getPath();System.out.println("""path  : String = """ + $show(path ));$skip(822); 
   
  def block(i: Int)(num: Int) = {
    println("Iteration: " + i.toString)
    
    val iter0 = (0 to (i+2) by 2).iterator
    val iter1 = (1 to (i+3) by 2).iterator
    val iterInf = Iterator.continually {
    Thread.sleep(100)
    1
    }
   
    val iter = i match {
      case 0  =>  iter0.flatMap( n => (n*10 to (n*10 + 5) by 1).iterator )
      case 1 => iter0.map( n => n*n )
      case 2 => iter0 ++ iter0 //this just returns iter0 - why is that?
      case 3 => iter0 ++ iter1
      case 4 => iter0.filter( n => n%4 == 0 )
      case 5 => iter0.take(3)
      case 6 => iter0.takeWhile(n => (n < 5))
      case 7 => iter0.zip(iter1)
      case 8 => ReadLinesFromDisk(path)
      case 9 => iterInf.filter( n => (n < 0) )
   }
   printOut(i)(iter)(num) //An iterator can't be traversed more than once.
       
	};System.out.println("""block: (i: Int)(num: Int)Unit""");$skip(358); 
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 3 toList).foreach(i =>block(i)(-1));$skip(96); 
  //breaking things up because output exceeded limit
  (4 to 8 toList).foreach(i =>block(i)(5));$skip(174); 

  // This step will never finish, even
  // though the filter is never satisfied
  // The worksheet can be terminated by closing it.
  //block(9)(num = 5)
  println("Done")}
    //blocking{Thread.sleep(3000)} // only needed for asynchronous worksheets
   
}
