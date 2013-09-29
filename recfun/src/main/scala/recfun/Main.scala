package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c > r) throw new IllegalArgumentException
    if (c == 0 || c == r) 1 else pascal(c-1,r-1) + pascal(c,r-1) 
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
	def balanceInternal(acc: Int, subchars: List[Char]): Boolean = {
	  if (acc < 0) false 
	  else if (subchars.isEmpty) acc == 0
	  else if (subchars.head == '(') balanceInternal(acc+1, subchars.tail)
	  else if (subchars.head == ')') balanceInternal(acc-1, subchars.tail)
	  else  balanceInternal(acc, subchars.tail)
	}	 
	balanceInternal(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
	  def sumOfCoins(accSum: Int, coins: List[Int], coefficients: List[Int]): Int = {
	   if (coefficients.isEmpty)
		if (accSum == money) 1
		else 0
	   else sumOfCoins(accSum + coins.head*coefficients.head, coins.tail, coefficients.tail)
	  }
	  
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
	  }
	  
	  def initialCoefficients(coins: List[Int], coefficients: List[Int]): List[Int] = {
	    if (coins.isEmpty) coefficients
	    else initialCoefficients(coins.tail, coefficients:+0)
	  }
	  
	  def totalCount(accCount: Int, coefficients: List[Int]): Int = {
	    if (coefficients.isEmpty) accCount
	    else totalCount(accCount + sumOfCoins(0, coins, coefficients), nextCoefficients(coins, coefficients, List(), 1)) 
	  }
	  
	  if (money <= 0|| coins.isEmpty) 0
	  else totalCount(0, initialCoefficients(coins, List())) 
  }
}
