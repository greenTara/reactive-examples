package observable

import math.random
import rx.lang.scala.Observable
import scala.concurrent._
import scala.io.Source
/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Review of Iterators
*/



object ob11 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

 def printOut[T](i:Int)(iter:Iterator[T]): Unit =
   while(iter.hasNext) {
      println("i = " + i.toString + ", next = " + iter.next().toString)
   }                                              //> printOut: [T](i: Int)(iter: Iterator[T])Unit
   
  def ReadLinesFromDisk(path: String): Iterator[String] = {
    Source.fromFile(path).getLines()
  }                                               //> ReadLinesFromDisk: (path: String)Iterator[String]
   
  val url = getClass().getResource("poem.txt")    //> url  : java.net.URL = file:/Users/taraathan/ScalaCoursera/Github/reactive-ex
                                                  //| amples/target/scala-2.10/classes/observable/poem.txt
  val path = url.getPath()                        //> path  : String = /Users/taraathan/ScalaCoursera/Github/reactive-examples/tar
                                                  //| get/scala-2.10/classes/observable/poem.txt
   
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
       
	}                                         //> block: (i: Int)Unit
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 7 toList).foreach(i =>block(i))           //> Iteration: 0
                                                  //| i = 0, next = 0
                                                  //| i = 0, next = 1
                                                  //| i = 0, next = 2
                                                  //| i = 0, next = 3
                                                  //| i = 0, next = 4
                                                  //| i = 0, next = 5
                                                  //| i = 0, next = 20
                                                  //| i = 0, next = 21
                                                  //| i = 0, next = 22
                                                  //| i = 0, next = 23
                                                  //| i = 0, next = 24
                                                  //| i = 0, next = 25
                                                  //| Iteration: 1
                                                  //| i = 1, next = 0
                                                  //| i = 1, next = 4
                                                  //| Iteration: 2
                                                  //| i = 2, next = 0
                                                  //| i = 2, next = 2
                                                  //| i = 2, next = 4
                                                  //| i = 2, next = 1
                                                  //| i = 2, next = 3
                                                  //| i = 2, next = 5
                                                  //| Iteration: 3
                                                  //| i = 3, next = 0
                                                  //| i = 3, next = 4
                                                  //| Iteration: 4
                                                  //| i = 4, next = 0
                                                  //| i = 4, next = 2
                                                  //| i = 4, next = 4
                                                  //| Iteration: 5
                                                  //| i = 5, next = 0
                                                  //| i = 5, next = 2
                                                  //| i = 5, next = 4
                                                  //| Iteration: 6
                                                  //| i = 6, next = (0,1)
                                                  //| i = 6, next = (2,3)
                                                  //| i = 6, next = (4,5)
                                                  //| i = 6, next = (6,7)
                                                  //| i = 6, next = (8,9)
                                                  //| Iteration: 7
                                                  //| i = 7, next = Two roads diverged in a yellow wood,
                                                  //| i = 7, next = And sorry I could not travel both
                                                  //| i = 7, next = And be on
                                                  //| Output exceeds cutoff limit.
   
    //blocking{Thread.sleep(3000)} // only needed for asynchronous worksheets
   
}