import math.random

/* This worksheet demonstrates some of the code snippets from
* Week3, Lecture 1, "Monads and Effects", particularly slides 3-6.
* This approach to implementing exceptions is not recommended
* because the exceptions (the "unhappy path") are not handled.
*/
object node1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  abstract class Coin {
     val denomination: Int
  }
    case class Silver() extends Coin {
      val denomination = 1
    }
    case class Gold() extends Coin {
      val denomination = 10
    }
    
  abstract class Treasure{
    val value: Int
    }
  
  trait Adventure {
    def collectCoins(): List[Coin]
    def buyTreasure(coins: List[Coin]): Treasure
  }
 
  def eatenByMonster(a:Adventure) = (random < 0.3)//> eatenByMonster: (a: node1.Adventure)Boolean
  
  class GameOverException(msg: String) extends Error{
    override def toString = msg
  }
  val treasureCost = 50                           //> treasureCost  : Int = 50
  
  object Diamond extends Treasure {
    val value = treasureCost
    override def toString = "Diamond"
  }
   
  def coinSource(rand: Double, prob: Double ): Coin =
    if (rand < prob) {
      Thread.sleep(100)
      new Gold
    }
    else {
      Thread.sleep(10)
      new Silver
    }                                             //> coinSource: (rand: Double, prob: Double)node1.Coin
  
  object AdventureFactory {
    /* The anonymous class syntax is used for this factory object,
    * allowing us to instantiate an object
    * that extends a trait having undefined members.
    * The anonymous class must provide definitions
    * for all undefined members of the trait.
    * Note that this object has an apply method:
    * AdventureFactory() is desugared to
    * AdventureFactory.apply()
    */
    def apply() = new Adventure {
       def collectCoins(): List[Coin] = {
         if (eatenByMonster(this))
           throw(new GameOverException("Oooops"))
         else for { i <- 1 to 10 toList } yield coinSource(random, 0.5)
       }
       def totalCoins(coins: List[Coin]) =
         coins.foldLeft(0)( (sum, coin) => sum + coin.denomination  )
       
       def buyTreasure(coins: List[Coin]): Treasure =
       {
         if (totalCoins(coins) < treasureCost)
           throw(new GameOverException("Nice try!"))
         else
           Diamond
       }
    }
  }
  
  def block(i: Int) = {
    println("Iteration: " + i.toString)
	  val adventure = AdventureFactory()
	  val coins = adventure.collectCoins()
	  val treasure = adventure.buyTreasure(coins)
	  println("Treasure: " + treasure.toString + " " + i.toString)
  }                                               //> block: (i: Int)Unit
  /* Multiple executions of a block of commands where
   * each block contains one collectCoins and
   * one buyTreasure. If either call fails, the whole iteration fails,
   * because we are not catching exceptions in this implementation.
   * Note that these blocks execute synchrounsly.
   */
  (1 to 10 toList).foreach(i =>block(i))          //> Iteration: 1
                                                  //| Oooops
                                                  //| 	at node1$$anonfun$main$1$AdventureFactory$2$$anon$1.collectCoins(node1.s
                                                  //| cala:64)
                                                  //| 	at node1$$anonfun$main$1.node1$$anonfun$$block$1(node1.scala:83)
                                                  //| 	at node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply$mcVI$sp(node1.sca
                                                  //| la:93)
                                                  //| 	at node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply(node1.scala:93)
                                                  //| 	at node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply(node1.scala:93)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:318)
                                                  //| 	at node1$$anonfun$main$1.apply$mcV$sp(node1.scala:93)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at node1$.main(node1.scala:8)
                                                  //| 	at node1.main(node1.scala)
  
   
}