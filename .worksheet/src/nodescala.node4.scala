package nodescala

import math.random
import scala.util.{Try, Success, Failure}

object node4 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(139); 
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
    def collectCoins(): Try[List[Coin]]
    def buyTreasure(coins: List[Coin]): Try[Treasure]
  };$skip(429); 
 
  def eatenByMonster(a:Adventure) = (random < 0.1)
  class GameOverException(msg: String) extends Error {
    override def toString = msg
  };System.out.println("""eatenByMonster: (a: nodescala.node4.Adventure)Boolean""");$skip(115); 
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
       def collectCoins(): Try[List[Coin]] = Try {
         if (eatenByMonster(this))
           throw(new GameOverException("Oooops"))
         else for { i <- 1 to 10 toList } yield coinSource(random, 0.5)
       }
       def totalCoins(coins: List[Coin]) =
         coins.foldLeft(0)( (sum, coin) => sum + coin.denomination  )
       
       def buyTreasure(coins: List[Coin]): Try[Treasure] = Try
       {
         if (totalCoins(coins) < treasureCost)
           throw(new GameOverException("Nice try!"))
         else
           Diamond
       }
    }
  };System.out.println("""coinSource: (rand: Double, prob: Double)nodescala.node4.Coin""");$skip(651); 

  val adventure = Adventure();System.out.println("""adventure  : nodescala.node4.Adventure{def totalCoins(coins: List[nodescala.node4.Coin]): Int} = """ + $show(adventure ));$skip(136); 
 val treasure: Try[Treasure] = for {
   coins <- adventure.collectCoins()
   treasure <- adventure.buyTreasure(coins)
 } yield treasure;System.out.println("""treasure  : scala.util.Try[nodescala.node4.Treasure] = """ + $show(treasure ))}
   
}
