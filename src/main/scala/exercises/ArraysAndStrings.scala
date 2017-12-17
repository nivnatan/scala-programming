package exercises

import scala.collection.mutable.ListBuffer

/**
  * Created by nivnatan on 11/19/2017.
  */
object ArraysAndStrings extends App {

  /**
    * Find Pair Of Integers in Array whose Sum is Given Number
    * Example:
    * {1, 2, 3,  4, 5} -> {2, 4} and {1, 5}
    * @param arr
    * @param sum
    */
  def arraySumPairs(arr: Array[Int], sum: Int): Unit = {
    // O(n^2)
    def arraySumPairsNaive = {
      for(i <- 0 until arr.length)
        for(j <- i until arr.length)
          if((arr(i) + arr(j)) == sum) print(s"{${arr(i)},${arr(j)}} ")
    }

    // O(n)
    def arraySumPairsEfficient = {
      val set = scala.collection.mutable.Set.empty[Int]
      arr.foreach { case elem =>
        val target = sum - elem
        if(set.contains(target)){
          print(s"{${elem},${target}} ")
        } else {
          set += elem
        }
      }
    }

    arraySumPairsEfficient
  }

  /**
    * @param str
    * @return true if string has all unique characters, otherwise false
    *         examples:
    *         isUnique("ascvfghj") = true
    *         isUnique("") = true
    *         isUnique("asdgdea") = false
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
    * Implement a method to perform basic string compression using the counts
    * of repeated characters. For example, the string aabcccccaaa would become a2b1c5a3. If the
    * "compressed" string would not become smaller than the original string, your method should return
    * the original string. You can assume the string has only uppercase and lowercase letters (a - z).
    * @param str
    * @return string compressed
    */
  def stringCompression(str: String): String = {
    def stringCompression(str: String, char: Char, count: Int, res: String): String = {
      if(str == "") res + char + count
      else {
        if(str.charAt(0) == char) {
          stringCompression(str.substring(1), char, count + 1, res)
        } else {
          stringCompression(str.substring(1), str.charAt(0), 1, res + char + count)
        }
      }
    }
    stringCompression(str.substring(1), str.charAt(0), 1, "")
  }

  /**
    * One Away: There are three types of edits that can be performed on strings: insert a character,
    * remove a character, or replace a character. Given two strings, write a function to check if they are
    * one edit (or zero edits) away.
    * EXAMPLE
    * pale, ple -> true
    * pales. pale -> true
    * pale. bale -> true
    * pale. bake -> false*
    * @param str1
    * @param str2
    * @return true/false
    */
  def oneWay(str1: String, str2: String): Boolean = {

    def removeEdit(strToRemove: String, strToCompare: String): Boolean = {
      var i = 0
      while(i < strToCompare.length && strToRemove.charAt(i) == strToCompare.charAt(i)) { i += 1 }
      if(i == strToCompare.length) true
      else (strToRemove.substring(0,i) + strToRemove.substring(i+1, strToRemove.length)) == strToCompare
    }

    def replaceEdit(strToReplace: String, strToCompare: String): Boolean = {
      var i = 0
      while(i < strToCompare.length && strToReplace.charAt(i) == strToCompare.charAt(i)) { i += 1 }
      if(i == strToCompare.length) true
      else {
        strToCompare.drop(i+1) == strToReplace.drop(i+1)
      }
    }

    (str1.length, str2.length) match {
      case (x,y) if(Math.abs(x - y) >= 2) => false
      case (x,y) if(x == y)               => replaceEdit(str1, str2)
      case (x,y) if(x > y)                => removeEdit(str1, str2)
      case (x,y) if(x < y)                => removeEdit(str2, str1)
    }
  }

  /**
    * @param str1
    * @param str2
    * @return true if two strings are permutations of each other, otherwise false
    *         examples:
    *         isPerm("abb", "aab") = false
    *         isPerm("abbc", "aabc") = false
    *         isPerm("abba", "aabb") = true
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
    * Given an array of positive numbers, find the highest sum of non-consecutive numbers. Example : [1, 2, 3, 4, 5] ! ! ! 1 + 3 + 5 = 9. Therefore, 9 is the answer.
    * 4 1 1 4 2 1 -> 4 + 4 + 1 = 9
    * https://www.youtube.com/watch?v=UtGtF6nc35g
    * @param arr
    * @return highest sum of non-consecutive numbers
    */
  def dynamicProgramming(arr: Array[Int]) = {
    var inclusive: Int = arr(0)
    var exclusive: Int = 0
    var temp: Int = 0

    for(i <- 1 until arr.length) {
      temp = inclusive
      inclusive = Math.max(inclusive, exclusive + arr(i))
      exclusive = temp
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
    * Rotate a string by n
    *
    * examples:
    * ("enforca",1) -> "aenforc"
    * ("enforca",2) -> "caenfor"
    *
    * @param str
    * @param n
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

  /**
    * Assume you have a method isSubst ring which checks if one word is a substring
    * of another. Given two strings, 51 and 52, write code to check if s2 is a rotation of s1 using only one
    * call to isSubstring (e.g., "waterbottle" is a rotation of"erbottlewat")
    *
    * @param str1
    * @param str2
    * @return true if s2 is a rotation of s1 using only one call to isSubstring
    */
  def stringRotation(str1: String, str2: String) = {

    def isSubstring(str1: String, str2: String) = str1.contains(str2)
    // the idea is the copy str1 and concat to itself as a new string. so "waterbottle" will become "waterbottlewaterbottle" and then check substring in one call
    if(str1.length != str2.length) false
    else isSubstring(str1 + str1, str2)
  }

  /**
    * Find the number which is not repeated in Array of integers, others are present for two times.
    * e.g. Input : 23, 34,56,21,21,56,78,23, 34
    * Output: 23
    *
    * @param arr
    * @return number which is not repeated in the array of integers
    */
  def stringRotation(arr: Array[Int]) = {
    // XOR
    arr.reduceLeft(_ ^ _)
  }
}
