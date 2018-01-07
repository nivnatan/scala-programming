package exercises

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by nivnatan on 11/19/2017.
  */
object ArraysAndStrings extends App {

  /**
    * Return array without the first and last element
    *
    * @param arr
    * @return new array without the first and second elements
    */
  def arrayWithoutFirstAndSecondElements(arr: Array[Int]): Int = {
    arr.init.head
  }

  /**
    * Find Pair Of Integers in Array whose Sum is Given Number
    * Example:
    * {1, 2, 3,  4, 5} -> {2, 4} and {1, 5}
    *
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
    * Reverse an array without using an additional space
    *
    * @param arr
    */
  def reverseArrayNoSpace(arr: Array[Int]) = {
    for(i <- 0 until arr.length / 2) {
      val temp = arr(i)
      arr(i) = arr(arr.length - 1 - i)
      arr(arr.length - 1 - i) = temp
    }
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
    *
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
    *
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
    *
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

  /**
    * Print all possible words from phone number digits.
    *
    * @param phoneNumber
    */
  def phoneNumbersToAllPossibleWords(phoneNumber: String): Unit = {
    def getCharsFromDigit(digit: Char): List[Char] = {
      digit match {
        case '0' => List('0')
        case '1' => List('1')
        case '2' => List('a','b','c')
        case '3' => List('d','e','f')
        case '4' => List('g','h','i')
        case '5' => List('j','k','l')
        case '6' => List('m','n','o')
        case '7' => List('p','q','r','s')
        case '8' => List('t','u','v')
        case '9' => List('w','x','y','z')
        case _   => Nil
      }
    }

    def go(phoneNumber: String, prefix: String = ""): Unit = {
      val digit                       = phoneNumber.charAt(0)
      val listWordChars               = getCharsFromDigit(digit)
      val phoneNumberExcludeFirstChar = phoneNumber.tail

      listWordChars.foreach { case char =>
        val currentCombination = prefix + char
        if(phoneNumber.length == 1) { // only if we are the deepest in the recursion
          print(currentCombination + " ")
        } else {
          go(phoneNumberExcludeFirstChar, prefix + char.toString)
        }
      }
    }
    go(phoneNumber)
  }

  /**
    * Move the Spaces to Front of the String
    * “move these spaces to beginning” => output =”    movethesepacestobeginning”
    *
    * @param string
    */
  def moveSpacesToFront(string: String): String = {
    val res = string.split(" ")
    ((for(_ <- 1 until res.length) yield " ").toArray ++ res).reduceLeft(_ + _)
  }

  /**
    * Move the Spaces to Front of the String
    * “move these spaces to beginning” => output =”    movethesepacestobeginning”
    *
    * @param string
    */
  def moveSpacesToFrontInPlaceArray(string: Array[Char]): Unit = {

    def roateSubStringOnce(indexOfSpace: Int) = {
      for(i <- indexOfSpace to 1 by -1) {
        string(i) = string(i - 1)
      }
      string(0) = ' '
    }

    def go(index: Int): Unit = {
      string.indexOf(' ', index) match {
        case -1  =>
        case num => {
          roateSubStringOnce(num)
          go(num)
        }
      }
    }
    go(0)
  }

  /**
    * Finding All the Subsets of a Set – Backtracking Problem
    *
    * @param set
    */
  def allSubsetsOfSets(set: Array[Char]): Unit = {
    // the idea is- the number of subsets equal to the 2^(number of elements in set)
    // {1,2,3} => 000, 001, 010, 011, 100, 101, 110, 111
    // we need to for loop 2^elements and then inner loop (number of elements) and check which "bit" is turned on, and print it (that way we cover all options)
    val results = for {
      i <- 0 until (1 << set.size)
      setRes = for {
        j <- 0 until set.size
        if(((i >> j) & 1) > 0)
      } yield set(j)
    } yield setRes

    print(results.map(_.mkString("{",",","}")).mkString("{"," ","}"))
  }

  /**
    * Determine if characters of a string follows a specified order or not
    * For example -
    * string = "Techie Delight", pattern = "el" => true {e,e,e,l} in order
    * string = "Techie Delight", pattern = "ei" => false {e,i,e,e,i} not in order
    *
    * @param string
    * @param pattern
    */
  def areAllCharactersFollowSpecifiedOrder(string: String, pattern: String): Boolean = {
    def areAllCharactersFollowSpecifiedOrderRec(index: Int): Boolean = {
      index match {
        case num if num == pattern.length => true
        case num                         => {
          val lastPrevCharOfPatternPosition     = string.lastIndexOf(pattern.charAt(index - 1))
          val firstCurrentCharOfPatternPosition = string.indexOf(pattern.charAt(index))
          if(lastPrevCharOfPatternPosition == -1 || firstCurrentCharOfPatternPosition == -1 || lastPrevCharOfPatternPosition > firstCurrentCharOfPatternPosition) false
          else {
            areAllCharactersFollowSpecifiedOrderRec(index + 1)
          }
        }
      }
    }
    // The idea is to loop via all pattern chars and check if the last position of previous char against first position of current char is smaller, otherwise false ({e,i,e,e,i} => last 'e' position is bigger then first 'i')
    pattern.length match {
      case 0 => true
      case 1 => string.indexOf(pattern.charAt(0)) > -1
      case _ => areAllCharactersFollowSpecifiedOrderRec(1)
    }
  }

  /**
    * Find two numbers with maximum sum formed by array digits (integers between 0 and 9). use all array digits, the difference in number of digits of the two numbers should be +=1
    * For example -
    * {4,6,2,7,9,8} => 974 + 862
    *
    * @param arr
    * @return (Int,Int)
    */
  def areAllCharactersFollowSpecifiedOrder(arr: Array[Int]): (Int,Int) = {
    def arrWithIndexToNumber(arr: Array[(Int, Int)]) = arr.foldLeft(0) { case (sum,(e,_)) => (sum * 10) + e }
    val (l1, l2) = arr.sortWith(_ > _).zipWithIndex.partition{ case (e,i) => (i % 2 == 0) } // {4,6,2,7,9,8} => {9,8,7,6,4,2} => {{9,7,4},{8,6,2}}
    (arrWithIndexToNumber(l1), arrWithIndexToNumber(l2)) // (974,862)
  }

  /**
    * Find ways to calculate target from elements of specified array
    * For example -
    * {5,3,-6,2} => -(-6), +5+3-2, +5-3-(-6)-2, -5+3-(-6)+2 => 4 ways
    *
    * @param arr
    * @return (Int,Int)
    */
  def numberOfWaysToCount(arr: Array[Int], target: Int): Int = {
    def numberOfWaysToCountRec(index: Int, sum: Int): Int = {
      if(sum == target) 1
      else if(index == arr.length) 0
      else {
        val exclude = numberOfWaysToCountRec(index + 1, sum) // without the first element
        exclude + numberOfWaysToCountRec(index + 1, sum - arr(index)) + numberOfWaysToCountRec(index + 1, sum + arr(index)) // check '-' and '+'
      }
    }
    numberOfWaysToCountRec(0, 0)
  }

  /**
    * How to find kth smallest element in unsorted array (better then sorting)?
    * Example - {1, 2, 3, 9, 4} and k=2 => return 2
    *
    * @param arr
    * @param k
    * @return k'th element
    */
  def findKthSmallestNumberInArray(arr: Array[Int], k: Int): Int = {
    object MinOrder extends Ordering[Int] {
      def compare(x: Int, y: Int) = y compare x
    }
    val minHeap = mutable.PriorityQueue.empty[Int](MinOrder)
    arr.foreach(minHeap += _)
    minHeap.take(k).last
  }

  /**
    * Find minimum index of a repeating element in an integer's array
    * Example - {5, 6, 3, 4, 3, 6, 4} => return 1 (6 is repeating)
    * Example - {5, 6, 3, 4}          => return -1
    *
    * @param arr
    * @return index of repeating element if exists, -1 otherwise
    */
  def findMinimumIndexOfRepeatingElement(arr: Array[Int]): Int = {
    def findMinimumIndexOfRepeatingElementRec(index: Int, indexMinimumRepeatable: Int, hash: Set[Int]): Int = {
      if(index < 0) indexMinimumRepeatable
      else {
        val element = arr(index)
        if(hash.contains(element)) findMinimumIndexOfRepeatingElementRec(index - 1, index, hash)
        else findMinimumIndexOfRepeatingElementRec(index - 1, indexMinimumRepeatable, hash + element)
      }
    }
    // the idea is to traverse the array from right to left
    findMinimumIndexOfRepeatingElementRec(arr.length -1, -1, Set.empty)
  }

  /**
    * Find count of distinct elements in every sub array of size k
    * Example - {1,2,3,3}, k=2 => {1,2}(2) + {2,3}(2) + {3,3}(1) => 5
    *
    * @param arr
    * @return index of repeating element if exists, -1 otherwise
    */
  def countDistinctElementsInEverySubArrayOfSizeK(arr: Array[Int], k: Int) = {
    // SUB ARRAYS MEANING THE ORDER COUNTS !!!!!
    def countDistinctElementsInEverySubArrayOfSizeKRec(index: Int, sum: Int): Int = {
      if(index + k > arr.size) sum
      else {
        val sumSubArray = (for { i <- index until index + k } yield arr(i)).toSet.size
        countDistinctElementsInEverySubArrayOfSizeKRec(index + 1, sum + sumSubArray)
      }
    }

    countDistinctElementsInEverySubArrayOfSizeKRec(0,0)
  }

  /**
    * Partition an array into two sub arrays with the same sum
    *
    * @param arr
    * @return Some(two sub arrays with the same sum) or None if not exists
    */
  def partitionArrayIntoTwoSubArraysWithTheSameSum(arr: Array[Int]) = {
    // SUB ARRAYS MEANING THE ORDER COUNTS !!!!!
    def partitionArrayIntoTwoSubArraysWithTheSameSumRec(index:Int, sumLeft: Int, sumOfAllArray: Int): Option[(Array[Int],Array[Int])] = {
      if(index >= arr.length -1) None
      else {
        // we can iterate and for each element to calculate the left and right sides, but for performance, we can save the already calculated left side and by pre-calculating the sum of array, we can know the sum of right side
        val sumLeftSide = arr(index) + sumLeft
        val sumRightSide = sumOfAllArray - sumLeftSide
        if(sumLeftSide == sumRightSide) Some(arr.splitAt(index + 1))
        else partitionArrayIntoTwoSubArraysWithTheSameSumRec(index + 1, sumLeftSide, sumOfAllArray)
      }
    }

    val sumOfAllArray = arr.reduce(_ + _)
    partitionArrayIntoTwoSubArraysWithTheSameSumRec(0, 0, sumOfAllArray)
  }

  /**
    * Generate random input from an array according to given probabilities
    * Example -
    * input:         {1,2,3,4,5}
    * probabilities: {30,10,20,15,25} // total probabilities should sum to 100%
    * the solution should return 1 with probability of 30%, 2 with 10%, ...
    *
    * @param arr
    * @return random input according to the the probabilities given
    */
  def generateRandomInputFromArrayAccordingToProbabilities(arr: Array[Int], prob: Array[Int]) = {
    require(arr.size == prob.size)
    require(prob.reduceLeft(_ + _) == 100)

    def findIndexInProbabilitiesArray(probabilitiesArray: Array[Int], index: Int, p: Int): Int = {
      if(probabilitiesArray(index) <= p && p <= probabilitiesArray(index + 1)) index
      else findIndexInProbabilitiesArray(probabilitiesArray, index + 1, p)
    }

    val probabilitiesArray = prob.scanLeft(0)((sum,p) => sum + p) // {0,30,40,60,75,100}
    val randomNumber = Random.nextInt(101)
    arr(findIndexInProbabilitiesArray(probabilitiesArray, 0, randomNumber))
  }
}
