package nodescala


import math.random
//import scala.util.{Try, Success, Failure}

object node3 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
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
  }
 
  def eatenByMonster(a:Adventure) = (random < 0.1)//> eatenByMonster: (a: nodescala.node3.Adventure)Boolean
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
      Thread.sleep(1000)
      new Gold
    }
    else {
      Thread.sleep(100)
      new Silver
    }                                             //> coinSource: (rand: Double, prob: Double)nodescala.node3.Coin
  
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

  val adventure = Adventure()                     //> adventure  : nodescala.node3.Adventure{def totalCoins(coins: List[nodescala
                                                  //| .node3.Coin]): Int} = nodescala.node3$$anonfun$main$1$Adventure$3$$anon$1@2
                                                  //| 009d3af
  val coins: Try[List[Coin]] = adventure.collectCoins()
                                                  //> coins  : nodescala.node3.Try[List[nodescala.node3.Coin]] = Success(List(Sil
                                                  //| ver(), Silver(), Gold(), Silver(), Silver(), Gold(), Silver(), Gold(), Gold
                                                  //| (), Silver()))
  val treasure: Try[Treasure] = coins.flatMap(cs=>{adventure.buyTreasure(cs)})
                                                  //> treasure  : nodescala.node3.Try[nodescala.node3.Treasure] = Failure(Nice tr
                                                  //| y!)


   
}