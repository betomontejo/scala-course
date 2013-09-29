package recfun

object Worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val money = 10;                                 //> money  : Int = 10
  
  def sumOfCoins(accSum: Int, coins: List[Int], coefficients: List[Int]): Int = {
   if (coefficients.isEmpty)
   		if (accSum == money) 1
   		else 0
   else sumOfCoins(accSum + coins.head*coefficients.head, coins.tail, coefficients.tail)
  }                                               //> sumOfCoins: (accSum: Int, coins: List[Int], coefficients: List[Int])Int
  
  sumOfCoins(0, List(1,2,5), List(50,0,10))       //> res0: Int = 0
  
  def nextCoefficients(coins: List[Int], oldCoefficients: List[Int], newCoefficients: List[Int], carry: Int): List[Int] = {
  	if (oldCoefficients.isEmpty)
  		if (carry > 0) List()
  		else newCoefficients
  	else {
  		val next = oldCoefficients.head + carry
  		if (next > money/coins.head)
  			nextCoefficients(coins.tail, oldCoefficients.tail, newCoefficients:+0, 1)
  		else
  			nextCoefficients(coins.tail, oldCoefficients.tail, newCoefficients:+next, 0)
  	}
  }                                               //> nextCoefficients: (coins: List[Int], oldCoefficients: List[Int], newCoeffici
                                                  //| ents: List[Int], carry: Int)List[Int]
                                                  
  nextCoefficients(List(1,2,5),List(10,5,1),List(), 1)
                                                  //> res1: List[Int] = List(0, 0, 2)
  
  
  def totalSum(accCount: Int, coefficients: List[Int]): Int = {
  	???
  }                                               //> totalSum: (accCount: Int, coefficients: List[Int])Int
  
	  def initialCoefficients(coins: List[Int], coefficients: List[Int]): List[Int] = {
	    if (coins.isEmpty) coefficients
	    else initialCoefficients(coins.tail, coefficients:+0)
	  }                                       //> initialCoefficients: (coins: List[Int], coefficients: List[Int])List[Int]
	  
	  initialCoefficients(List(1,5,3,2), List())
                                                  //> res2: List[Int] = List(0, 0, 0, 0)
   import recfun.Main._
   
   //countChange(525, List(5,10,20,50,100,200,500))
   
   "".toList                                      //> res3: List[Char] = List()
   

}