package nodescala


import math.random
//import scala.util.{Try, Success, Failure}

object node3 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(142); 
  println("Welcome to the Scala worksheet")
  
  abstract class Try[+T] {
      def flatMap[S](f: T=>Try[S]): Try[S] = this match {
		    case Success(value)   => f(value)
		    //case Success(value)   => Try(f(value))
		    //case Success(value)   => try {f(value) } catch { case t:Throwable => Failure(t)}
		    case failure @ Failure(t)        => Failure(t)
		  }
  }
  
  case class Success[+T](elem: T) extends Try[T]
  
  case class Failure(t: Throwable) extends Try[Nothing]
  
  object Try {
    def apply[T](r: =>T): Try[T] = {

		  
      try { Success(r) }
      catch { case t:Throwable => Failure(t) }
  
    }
  }
  
  
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
  };$skip(1019); 
 
  def eatenByMonster(a:Adventure) = (random < 0.3)
  class GameOverException(msg: String) extends Error{
    override def toString = msg
  };System.out.println("""eatenByMonster: (a: nodescala.node3.Adventure)Boolean""");$skip(114); 
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
  };System.out.println("""coinSource: (rand: Double, prob: Double)nodescala.node3.Coin""");$skip(970); 
  def block() = {
	  val adventure = Adventure()
	  val coins: Try[List[Coin]] = adventure.collectCoins()
	  val treasure: Try[Treasure] = coins.flatMap(cs=>{adventure.buyTreasure(cs)})
	  treasure match {
	    case Success(tr)     => println("Treasure: " ++ tr.toString)
	    case Failure(t)      => println("Error Message: " ++ t.toString)
	  }
	};System.out.println("""block: ()Unit""");$skip(40); 
  (1 to 10 toList).foreach(e =>block())}


   
}
