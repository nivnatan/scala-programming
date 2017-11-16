import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by niv on 16/11/2017.
  */
object Exercises extends App {

  /**
    * @param str
    * @return true if string has all unique characters, otherwise false
    * examples:
      isUnique("ascvfghj") = true
      isUnique("") = true
      isUnique("asdgdea") = false
    */
  def isUnique(str: String): Boolean = {
    val set = scala.collection.mutable.Set.empty[Char]
    if(str == "") true
    else {
      str.forall { case c =>
        set.contains(c) match {
          case true  => false
          case false => {
            set += c
            true
          }
        }
      }
    }
  }

  /**
    * @param str1
    * @param str2
    * @return true if two strings are permutations of each other, otherwise false
    * examples:
      isPerm("abb", "aab") = false
      isPerm("abbc", "aabc") = false
      isPerm("abba", "aabb") = true
    */
  def isPerm(str1: String, str2: String): Boolean = {
    // possible also to sort both strings and compare
    if (str1.length == str2.length) {
      val map1 = str1.groupBy(identity).mapValues(_.length)
      val map2 = str2.groupBy(identity).mapValues(_.length)
      map1.forall { case (c, count) => map2.get(c).map(_ == count).getOrElse(false) }
    } else {
      false
    }
  }

  /**
    * Write a method to replace all spaces in a string with '%20'
    *
    * examples:
    * urlify("a dss affa efg") = "a%20dss%20affa%20efg"
    *
    * @param str
    * @return new string with spaces replaced by '%20'
    */
  def urlify(str: String): String = {
    str.foldLeft("")((a,b) => a + (if(b == ' ') "%20" else b.toString))
  }

  /**
    * Write a method to check if a given string has palindrome permutations
    *
    * examples:
    * palPerm("tact coa") = true ("taco cat")
    *
    * @param str
    * @return boolean
    */
  def palPerm(str: String): Boolean = {
    str.permutations.toArray.map(_.replace(" ", "")).exists(perm => perm.reverse == perm)
  }

  /**
    * Implement wildcard pattern matching for '?' (Exactly one) and '*' (Zero/One/Many) Do not use regex.
    *
    * examples:
    * isMatch("aa","a") → false
    * isMatch("aa","aa") → true
    * isMatch("aaa","aa") → false
    * isMatch("aa", "*") → true
    * isMatch("aa", "a*") → true
    * isMatch("ab", "?*") → true
    * isMatch("aab", "c*a*b") → false
    *
    * @param str, str
    * @return boolean
    */
  def isMatchingSeq(str: Seq[Char], pattern: Seq[Char]): Boolean = {
    (str, pattern) match {
      case (Nil, Nil) => true
      case (sHead :: sTail, pHead :: pTail) if sHead == pHead || pHead == "?" =>
        isMatchingSeq(sTail, pTail)
      case (_ :: sTail, pHead :: pTail) if pHead == "*" =>
        isMatchingSeq(sTail, pTail) || isMatchingSeq(sTail, pattern) || isMatchingSeq(str, pTail)
      case _ => false
    }
  }

  /**
    * Given a string make sure that parentheses '(', ')' are opened/closed in the correct order
    *
    * examples:
    * "((()))"    -> true
    * "((())"     -> false
    * ("(())"     -> true
    * ")("        -> false
    * "))(("      -> false
    * "((()())"   -> false
    * "((()()))"  -> true
    *
    * @param str
    * @return boolean
    */
  def validateParen(str: String): Boolean = {
    def rec(chars: List[Char], count: Int): Boolean = {
      chars match {
        case Nil if(count == 0) => true
        case '(' :: tail => rec(tail, count + 1)
        case ')' :: tail if(count >= 1) => rec(tail, count - 1)
        case _ => false
      }
    }
    rec(str.toList, 0)
  }

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
    * @param number1,number2
    * @return gcd of number1 and number2
    */

  def gcd(number1: Int, number2: Int): Int = {
    if(number2 == 0) number1
    else gcd(number2, number1 % number2)
  }

  /**
    * Rotate a string by n
    *
    * examples:
    * ("enforca",1) -> "aenforc"
    * ("enforca",2) -> "caenfor"
    *
    * @param str, n
    * @return string rotated n times
    */
  def rotateAStringByN(str: String, n: Int): String = {

    def withoutBuffer = {
      val takeFrom = n % str.length
      val a = str.takeRight(takeFrom)
      val b = str.take(takeFrom)
      val c = str.substring(takeFrom, str.length - takeFrom)
      a + b + c
    }

    def withBuffer = {
      val sb = new Array[Char](str.length)
      for(i <- 0 until str.length) {
        val pos = (i + n) % str.length
        sb(pos) = str.charAt(i)
      }
      sb.foldLeft("")((a,b) => a + b.toString)
    }
    withoutBuffer
  }

  /**
    * Reverse a string
    *
    * examples:
    * ("abcd") -> "dcba"
    *
    * @param str
    * @return reversed string
    */
  def stringReverse(str: String) = {
    val b = Array.newBuilder[Char]
    var i = str.length
    while (0 < i) {
      i -= 1
      b += str(i)
    }
    b.result()
  }

  /**
    * Write an algorithm such that if an element in an MxN matrix is 0, its entire row and column is set to 0.
    *
    * examples:
    * 1	  2	  0	  4
    * 5	  6	  7	  8
    * 9	  10	11	12
    * 13	14	15	16
    *
    * will return as :
    *
    * 0	  0	  0	  0
    * 5	  6	  0	  8
    * 9	  10	0	  12
    * 13	14	0	  16
    *
    * @param matrix
    * @return Array[Array[Int]
    */
  def zeroMatrix(matrix:  Array[Array[Int]]): Array[Array[Int]] = {
    val rows = matrix.length
    val cols = matrix.headOption.map(_.length).getOrElse(0)
    var rowsBuffer = new ListBuffer[Int]()
    var colsBuffer = new ListBuffer[Int]()

    def resetCol(colNum: Int) = {
      for(i <-0 until rows) {
        matrix(i)(colNum) = 0
      }
    }

    def resetRow(rowNum: Int) = {
      for(i <-0 until cols) {
        val adsas = matrix(rowNum)(i)
        matrix(rowNum)(i) = 0
      }
    }

    // map all rows and cols indexes
    for(i <-0 until rows) {
      for (j <- 0 until cols) {
        if (matrix(i)(j) == 0) {
          rowsBuffer += i
          colsBuffer += j
        }
      }
    }

    rowsBuffer.foreach(resetRow) // for each row mapped index - reset all row
    colsBuffer.foreach(resetCol) // for each col mapped index - reset all col

    matrix
  }

  def printMatrix(matrix:  Array[Array[Int]]) = {
    val rows = matrix.length
    val cols = matrix.headOption.map(_.length).getOrElse(0)
    for(i <-0 until rows) {
      for(j <- 0 until cols) {
        print(matrix(i)(j))
        print('\t')
      }
      print('\n')
    }
  }
}
