import math.random

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
  
  object Adventure {
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
	  val adventure = Adventure()
	  val coins = adventure.collectCoins()
	  val treasure = adventure.buyTreasure(coins)
	  println("Treasure: " + treasure.toString + " " + i.toString)
  }                                               //> block: (i: Int)Unit
  (1 to 10 toList).foreach(i =>block(i))          //> Iteration: 1
                                                  //| Treasure: Diamond 1
                                                  //| Iteration: 2
                                                  //| Oooops
                                                  //| 	at node1$$anonfun$main$1$Adventure$3$$anon$1.collectCoins(node1.scala:50
                                                  //| )
                                                  //| 	at node1$$anonfun$main$1.node1$$anonfun$$block$1(node1.scala:69)
                                                  //| 	at node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply$mcVI$sp(node1.sca
                                                  //| la:73)
                                                  //| 	at node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply(node1.scala:73)
                                                  //| 	at node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply(node1.scala:73)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:318)
                                                  //| 	at node1$$anonfun$main$1.apply$mcV$sp(node1.scala:73)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at node1$.main(node1.scala:3)
                                                  //| 	at node1.
                                                  //| Output exceeds cutoff limit.
  
   
}