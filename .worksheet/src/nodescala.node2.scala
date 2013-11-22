package nodescala


import math.random
import scala.util.{Try, Success, Failure}

object node2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(140); 
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
 
  def eatenByMonster(a:Adventure) = (random < 0.3)
  class GameOverException(msg: String) extends Error{
    override def toString = msg
  };System.out.println("""eatenByMonster: (a: nodescala.node2.Adventure)Boolean""");$skip(114); 
  val treasureCost = 50
  
  object Diamond extends Treasure {
    val value = treasureCost
    override def toString = "Diamond"
  };System.out.println("""treasureCost  : Int = """ + $show(treasureCost ));$skip(293); 
   
  def coinSource(rand: Double, prob: Double ): Coin =
    if (rand < prob) {
      Thread.sleep(100)
      new Gold
    }
    else {
      Thread.sleep(10)
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
  };System.out.println("""coinSource: (rand: Double, prob: Double)nodescala.node2.Coin""");$skip(1118); 
  
  def block() = {
	  val adventure = Adventure()
	  val coins: Try[List[Coin]] = adventure.collectCoins()
	  val treasure: Try[Treasure] = coins match {
	   case Success(cs)          => adventure.buyTreasure(cs)
	   //case failure @ Failure(t) => failure  // This produces a type error.
	   case Failure(t) => Failure(t)
	  }
	  
	  treasure match {
	    case Success(tr)     => println("Treasure: " ++ tr.toString)
	    case Failure(t)      => println("Error Message: " ++ t.toString)
	  }
  };System.out.println("""block: ()Unit""");$skip(40); 
  (1 to 10 toList).foreach(e =>block())}
}
