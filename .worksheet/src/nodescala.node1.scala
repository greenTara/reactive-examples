package nodescala


import math.random

object node1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(98); 
  println("Welcome to the Scala worksheet")
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
  };$skip(419); 
 
  def eatenByMonster(a:Adventure) = (random < 0.1)
  class GameOverException(msg: String) extends Error;System.out.println("""eatenByMonster: (a: nodescala.node1.Adventure)Boolean""");$skip(77); 
  val treasureCost = 50
  
  object Diamond extends Treasure {
    val value = treasureCost
    override def toString = "Diamond"
  };System.out.println("""treasureCost  : Int = """ + $show(treasureCost ));$skip(295); 
   
  def coinSource(rand: Double, prob: Double ): Coin =
    if (rand < prob) {
      Thread.sleep(1000)
      new Gold
    }
    else {
      Thread.sleep(100)
      new Silver
    }
  
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
  };System.out.println("""coinSource: (rand: Double, prob: Double)nodescala.node1.Coin""");$skip(633); 

  val adventure = Adventure();System.out.println("""adventure  : nodescala.node1.Adventure{def totalCoins(coins: List[nodescala.node1.Coin]): Int} = """ + $show(adventure ));$skip(39); 
  val coins = adventure.collectCoins();System.out.println("""coins  : List[nodescala.node1.Coin] = """ + $show(coins ));$skip(46); 
  val treasure = adventure.buyTreasure(coins);System.out.println("""treasure  : nodescala.node1.Treasure = """ + $show(treasure ))}
   
   
}
