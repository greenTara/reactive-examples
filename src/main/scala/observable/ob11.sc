package observable

import scala.language.postfixOps
import scala.io.Source
/* This worksheet demonstrates some of the code snippets from
* Week4, Lecture 1, "Futures to Observables".
* Review of Iterators
*/

object ob11 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

 def printOut[T](i:Int)(iter:Iterator[T])(num:Int): Unit = {
   val iterP =
     if (num > 0 ) iter.take(num)
     else iter
   while(iterP.hasNext) {
      println("i = " + i.toString + ", next = " + iterP.next().toString)
   }
  }                                               //> printOut: [T](i: Int)(iter: Iterator[T])(num: Int)Unit
   
  def ReadLinesFromDisk(path: String): Iterator[String] = {
    Source.fromFile(path).getLines()
  }                                               //> ReadLinesFromDisk: (path: String)Iterator[String]
   
  val url = getClass().getResource("poem.txt")    //> url  : java.net.URL = file:/Users/taraathan/ScalaCoursera/Github/reactive-ex
                                                  //| amples/target/scala-2.10/classes/observable/poem.txt
  val path = url.getPath()                        //> path  : String = /Users/taraathan/ScalaCoursera/Github/reactive-examples/tar
                                                  //| get/scala-2.10/classes/observable/poem.txt
   
  def block(i: Int)(num: Int) = {
    println("Iterable: " + i.toString)
    
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
       
	}                                         //> block: (i: Int)(num: Int)Unit
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration does not fail,
   * because we are catching exceptions (with flatMap) in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (0 to 3 toList).foreach(i =>block(i)(-1))       //> Iterable: 0
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
                                                  //| Iterable: 1
                                                  //| i = 1, next = 0
                                                  //| i = 1, next = 4
                                                  //| Iterable: 2
                                                  //| i = 2, next = 0
                                                  //| i = 2, next = 2
                                                  //| i = 2, next = 4
                                                  //| Iterable: 3
                                                  //| i = 3, next = 0
                                                  //| i = 3, next = 2
                                                  //| i = 3, next = 4
                                                  //| i = 3, next = 1
                                                  //| i = 3, next = 3
                                                  //| i = 3, next = 5
  //breaking things up because output exceeded limit
  (4 to 8 toList).foreach(i =>block(i)(5))        //> Iterable: 4
                                                  //| i = 4, next = 0
                                                  //| i = 4, next = 4
                                                  //| Iterable: 5
                                                  //| i = 5, next = 0
                                                  //| i = 5, next = 2
                                                  //| i = 5, next = 4
                                                  //| Iterable: 6
                                                  //| i = 6, next = 0
                                                  //| i = 6, next = 2
                                                  //| i = 6, next = 4
                                                  //| Iterable: 7
                                                  //| i = 7, next = (0,1)
                                                  //| i = 7, next = (2,3)
                                                  //| i = 7, next = (4,5)
                                                  //| i = 7, next = (6,7)
                                                  //| i = 7, next = (8,9)
                                                  //| Iterable: 8
                                                  //| i = 8, next = Two roads diverged in a yellow wood,
                                                  //| i = 8, next = And sorry I could not travel both
                                                  //| i = 8, next = And be one traveler, long I stood
                                                  //| i = 8, next = And looked down one as far as I could
                                                  //| i = 8, next = To where it bent in the undergrowth;

  // This step will never finish, even
  // though the filter is never satisfied
  // The worksheet can be terminated by closing it.
  //block(9)(num = 5)
  println("Done")                                 //> Done
   
}