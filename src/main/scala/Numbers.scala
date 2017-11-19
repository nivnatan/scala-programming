import scala.annotation.tailrec

/**
  * Created by nivnatan on 11/19/2017.
  */
class Numbers {

  /**
    * Factorial of n
    *
    * examples:
    * 5 -> 120
    *
    * @param number
    * @return factorial result
    */
  def factorialRec(number: Int): Int = {
    if(number == 1) 1
    else number * factorialRec(number - 1)
  }

  def factorialTailRec(number: Int): Int = {
    @tailrec
    def go(number: Int, sum: Int): Int = {
      if(number == 1) sum
      else go(number - 1, sum * number)
    }
    go(number, 1)
  }

  def factorialIter1(number: Int): Int = {
    if(number == 1) 1
    (for(i <- number until 0 by -1) yield i).reduceLeft(_ * _)
  }

  def factorialIter2(number: Int): Int = {
    var sum = 1
    var numberTemp = number
    while(numberTemp > 1) {
      sum = sum * numberTemp
      numberTemp += -1
    }
    sum
  }

  /**
    * GCD
    *
    * algorithm - if y is zero then return x, else x = x % y
    *
    * examples:
    * 25,54 -> 6
    *
    * @param number1
    * @param number2
    * @return gcd of number1 and number2
    */

  def gcd(number1: Int, number2: Int): Int = {
    if(number2 == 0) number1
    else gcd(number2, number1 % number2)
  }

}
