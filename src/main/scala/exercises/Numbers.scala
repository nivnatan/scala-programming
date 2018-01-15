package exercises

import scala.annotation.tailrec

/**
  * Created by nivnatan on 11/19/2017.
  */
object Numbers extends App {

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

  /**
    * Print 1 to 100 without using loops or conditions
    */
  def printNoCondition: Unit = {
    def printNoConditionRec(num: Int): Unit = {
      try {
        1 / (101 - num) // will break when division by zero occurs
        print(num + " ")
        printNoConditionRec(num + 1)

      } catch { case e: ArithmeticException => }
    }
    printNoConditionRec(1)
  }

  def isPrime(number: Int) = {
    number >= 2 && (2 to math.sqrt(number).toInt).forall(number %_ != 0)
  }

  /**
    * Print all prime numbers until a given N
    */
  def printAllPrimeNumbersUntilN(n: Int): Unit = {
    Range(2, n).foreach(x => if(isPrime(x)) print(x))
  }

  /**
    * Print all prime numbers until a given N
    */
  def getAListOfPrimesUntilN(n: Int): List[Int] = {
    Range(2, n+1).filter(isPrime).toList
  }

  /**
    * Print all prime numbers until a given N
    * Another approach would be to use the following algorithm described in https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Example
    */
  def getAListOfPrimesUntilNUsingStreams(n: Int): List[Int] = {
    // Construct a stream consisting of a given first element = 2 followed by elements
    // from a lazily evaluated Stream. this jumps by 2 as no need to check even numbers against prime function
    val primes = 2 #:: Stream.from(3,2).filter(isPrime)
    val result = primes.takeWhile(_ <= n).toList
    result
  }

  def printNPrimeNumbers(n: Int): Unit = {
    def printNPrimeNumbersRec(counter: Int, number: Int): Unit = {
      if(counter < n) {
        if(isPrime(number)) {
          println(number)
          printNPrimeNumbersRec(counter + 1, number + 1)
        } else {
          printNPrimeNumbersRec(counter, number + 1)
        }
      }
    }
    if(n > 0) printNPrimeNumbersRec(0, 2)
  }
}
