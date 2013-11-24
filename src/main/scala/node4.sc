import math.random
import scala.util.{Try, Success, Failure}

object node4 {
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
    def collectCoins(): Try[List[Coin]]
    def buyTreasure(coins: List[Coin]): Try[Treasure]
  }
 
  def eatenByMonster(a:Adventure) = (random < 0.3)//> eatenByMonster: (a: node4.Adventure)Boolean
  class GameOverException(msg: String) extends Error {
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
    }                                             //> coinSource: (rand: Double, prob: Double)node4.Coin
  
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
  }

  def block(i: Int) = {
    println("Iteration: " + i.toString)
	  val adventure = Adventure()
	  val treasure: Try[Treasure] = for {
	    coins <- adventure.collectCoins()
	    treasure <- adventure.buyTreasure(coins)
	  } yield treasure
	   treasure match {
	    case Success(tr)     => println("Treasure: " + tr.toString + " " + i.toString)
	    case Failure(t)      => println("Error Message: " + t.toString + " " + i.toString)
	  }
	  
  }                                               //> block: (i: Int)Unit
 (1 to 10 toList).foreach(i =>block(i))           //> Iteration: 1
                                                  //| Treasure: Diamond 1
                                                  //| Iteration: 2
                                                  //| Treasure: Diamond 2
                                                  //| Iteration: 3
                                                  //| Error Message: Oooops 3
                                                  //| Iteration: 4
                                                  //| Treasure: Diamond 4
                                                  //| Iteration: 5
                                                  //| Error Message: Oooops 5
                                                  //| Iteration: 6
                                                  //| Treasure: Diamond 6
                                                  //| Iteration: 7
                                                  //| Treasure: Diamond 7
                                                  //| Iteration: 8
                                                  //| Treasure: Diamond 8
                                                  //| Iteration: 9
                                                  //| Error Message: Oooops 9
                                                  //| Iteration: 10
                                                  //| Error Message: Nice try! 10
   
}