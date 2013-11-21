package nodescala

import math.random

object node {
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
 
  def eatenByMonster(a:Adventure) = (random < 0.1)//> eatenByMonster: (a: nodescala.node.Adventure)Boolean
  class GameOverException(msg: String) extends Error
  val treasureCost = 50                           //> treasureCost  : Int = 50
  
  object Diamond extends Treasure {
    val value = treasureCost
  }
   
  def coinSource(rand: Double, prob: Double ): Coin = if (rand < prob) new Gold else new Silver
                                                  //> coinSource: (rand: Double, prob: Double)nodescala.node.Coin
  
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

  val adventure = Adventure()                     //> adventure  : nodescala.node.Adventure{def totalCoins(coins: List[nodescala.
                                                  //| node.Coin]): Int} = nodescala.node$$anonfun$main$1$Adventure$3$$anon$1@185a
                                                  //| fba1
  val coins = adventure.collectCoins()            //> coins  : List[nodescala.node.Coin] = List(Silver(), Silver(), Silver(), Sil
                                                  //| ver(), Gold(), Gold(), Silver(), Gold(), Silver(), Silver())
  val treasure = adventure.buyTreasure(coins)     //> nodescala.node$$anonfun$main$1$GameOverException$1
                                                  //| 	at nodescala.node$$anonfun$main$1$Adventure$3$$anon$1.buyTreasure(nodesc
                                                  //| ala.node.scala:49)
                                                  //| 	at nodescala.node$$anonfun$main$1.apply$mcV$sp(nodescala.node.scala:58)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at nodescala.node$.main(nodescala.node.scala:5)
                                                  //| 	at nodescala.node.main(nodescala.node.scala)
   
   
}