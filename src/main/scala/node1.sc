package nodescala

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
 
  def eatenByMonster(a:Adventure) = (random < 0.3)//> eatenByMonster: (a: nodescala.node1.Adventure)Boolean
  
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
    }                                             //> coinSource: (rand: Double, prob: Double)nodescala.node1.Coin
  
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
  
  def block() = {
	  val adventure = Adventure()
	  val coins = adventure.collectCoins()
	  val treasure = adventure.buyTreasure(coins)
	  println("Treasure: " ++ treasure.toString)
  }                                               //> block: ()Unit
  (1 to 10 toList).foreach(e =>block())           //> Treasure: Diamond
                                                  //| Treasure: Diamond
                                                  //| Oooops
                                                  //| 	at nodescala.node1$$anonfun$main$1$Adventure$3$$anon$1.collectCoins(node
                                                  //| scala.node1.scala:52)
                                                  //| 	at nodescala.node1$$anonfun$main$1.nodescala$node1$$anonfun$$block$1(nod
                                                  //| escala.node1.scala:70)
                                                  //| 	at nodescala.node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply$mcVI$sp
                                                  //| (nodescala.node1.scala:74)
                                                  //| 	at nodescala.node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply(nodesca
                                                  //| la.node1.scala:74)
                                                  //| 	at nodescala.node1$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply(nodesca
                                                  //| la.node1.scala:74)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:318)
                                                  //| 	at nodescala.node1$$anonfun$main$1.apply$mcV$sp(nodescala.node1.scala:74
                                                  //| )
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaid
                                                  //| Output exceeds cutoff limit.
  
   
}