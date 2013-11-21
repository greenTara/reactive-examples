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
 
  def eatenByMonster(a:Adventure) = (random < 0.1)//> eatenByMonster: (a: nodescala.node1.Adventure)Boolean
  class GameOverException(msg: String) extends Error
  val treasureCost = 50                           //> treasureCost  : Int = 50
  
  object Diamond extends Treasure {
    val value = treasureCost
    override def toString = "Diamond"
  }
   
  def coinSource(rand: Double, prob: Double ): Coin =
    if (rand < prob) {
      Thread.sleep(1000)
      new Gold
    }
    else {
      Thread.sleep(100)
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

  val adventure = Adventure()                     //> adventure  : nodescala.node1.Adventure{def totalCoins(coins: List[nodescala
                                                  //| .node1.Coin]): Int} = nodescala.node1$$anonfun$main$1$Adventure$3$$anon$1@2
                                                  //| ee28063
  val coins = adventure.collectCoins()            //> coins  : List[nodescala.node1.Coin] = List(Gold(), Gold(), Gold(), Silver()
                                                  //| , Gold(), Silver(), Silver(), Gold(), Silver(), Gold())
  val treasure = adventure.buyTreasure(coins)     //> treasure  : nodescala.node1.Treasure = Diamond
   
   
}