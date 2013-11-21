package nodescala

import math.random

object node {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(96); 
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
  class GameOverException(msg: String) extends Error;System.out.println("""eatenByMonster: (a: nodescala.node.Adventure)Boolean""");$skip(77); 
  val treasureCost = 50
  
  object Diamond extends Treasure {
    val value = treasureCost
  };System.out.println("""treasureCost  : Int = """ + $show(treasureCost ));$skip(172); 
   
  def coinSource(rand: Double, prob: Double ): Coin = if (rand < prob) new Gold else new Silver
  
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
  };System.out.println("""coinSource: (rand: Double, prob: Double)nodescala.node.Coin""");$skip(629); 

  val adventure = Adventure();System.out.println("""adventure  : nodescala.node.Adventure{def totalCoins(coins: List[nodescala.node.Coin]): Int} = """ + $show(adventure ));$skip(39); 
  val coins = adventure.collectCoins();System.out.println("""coins  : List[nodescala.node.Coin] = """ + $show(coins ));$skip(46); 
  val treasure = adventure.buyTreasure(coins);System.out.println("""treasure  : nodescala.node.Treasure = """ + $show(treasure ))}
   
   
}
