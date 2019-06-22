package exercises

import java.util

import exercises.StackAndQueues.{MyQueue, MyStack}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
  def arrayWithoutFirstAndSecondElements(arr: Array[Int]): Array[Int] = {
    arr.init.tail
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
    // build res (string) as we go to produce final result
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
  def isMatchingStr(str: String, pattern: String): Boolean = {
    (str, pattern) match {
      // If we reach at the end of both strings, we are done
      case ("","")   => true
      // special case where the str has ended and the pattern is still valid as it is '*'
      case ("", p)   => p == "*"
      // If pattern is empty and str is not, it means that the str does not follow the pattern
      case (str, "") => false
      // If the first string contains '?', or current characters of both strings match move forward
      case (str, p) if(str.charAt(0) == p.charAt(0) || p.charAt(0) == '?') => isMatchingStr(str.tail, p.tail)
      // If there is *, then there are two possibilities
      // a) We consider current character of second string
      // b) We ignore current character of second string.
      case (str, p) if(p.charAt(0) == '*') => isMatchingStr(str.tail, p) || isMatchingStr(str, p.tail)
      // otherwise and to catch non matched characters between str and pattern, return false
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
    * Reverse a string recursively
    *
    * examples:
    * ("abcd") -> "dcba"
    *
    * @param str
    * @return reversed string
    */
  def stringReverseRec(str: String): String = {
    if(str == "") ""
    else {
      val previousString = stringReverseRec(str.drop(1))
      previousString + str.charAt(0)
    }
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

  /**
    * Given a sequence of numbers such that the difference between the consecutive terms is constant, find missing term in it in log(n) time.
    * Example -
    * {5,7,9,11,13,15,19} missing term is 17
    * {1,4,7,13,16} missing term is 10
    *
    * @param arr
    * @return missing term in sequence
    */
  def findMissingTermInSequenceInLogNTime(arr: Array[Int]) = {
    // perform binary search
    def findMissingTermInSequenceInLogNTimeRec(low: Int, high: Int, term: Int): Int = {
      val mid = high - (high - low) / 2
      // check left side
      if(mid-1 > 0 && arr(mid) - arr(mid-1) != term) {
        arr(mid) - term
      }
      // check right side
      else if(mid+1 < arr.size && arr(mid + 1) - arr(mid) != term) {
        arr(mid) + term
      } else {
        // check if the missing term is somewhere on the left side. for example, for {5,7,9,11,13,15,19} with mid = 3, the missing term is not on the left side
        if(arr(mid) - arr(0) != mid * term) findMissingTermInSequenceInLogNTimeRec(low, mid-1, term)
        else findMissingTermInSequenceInLogNTimeRec(mid+1, high, term)
      }
    }
    // calculate term = (last element - first element) / n (number of elements in the array considering on element is missing in the sequence, otherwise it's n-1)
    val term = (arr(arr.size - 1) - arr(0)) / arr.size
    findMissingTermInSequenceInLogNTimeRec(0, arr.size-1, term)
  }

  /**
    * Division of two decimal numbers using binary search
    *
    * @param d1
    * @param d2
    * @return division result
    */
  def divideUsingBinarySearch(d1: Double, d2: Double) = {
    val d1Abs = Math.abs(d1)
    val d2Abs = Math.abs(d2)
    val precision = 0.001

    // perform binary search
    def divideUsingBinarySearchRec(low: Double, high: Double): Double = {
      val mid = high - (high - low) / 2

      if(Math.abs((d2Abs * mid) - d1Abs) <= precision) {
        mid
      }
      else if(d2Abs * mid > d1Abs) {
        divideUsingBinarySearchRec(low, mid)
      }
      else {
        divideUsingBinarySearchRec(mid, high)
      }
    }

    val sign = if(d1*d2 < 0) -1 else 1
    divideUsingBinarySearchRec(0, Double.MaxValue) * sign
  }

  /**
    * Two words are said to be anagrams of each other if they are made up of same characters
    * if we have an array of words, then we want to group the words of one anagram together.
    * For example -
    * For input array {"ram", "pot", "mar", "top", "arm"}, the output array should be {"ram", "mar", "arm", "pot", "top"}
    *
    * @param arr
    * @return array output
    */
  def anagrams(arr: Array[String]): Array[String] = {
    arr.map(str => (str,str.toSet)).groupBy { case (word,set) => set }.values.map { case arr => arr.map { case (word,set) => word }}.flatten.toArray
  }

  /**
    * Given an array having even number of integers.
    * Find if the array has N / 2 pairs of integers such that each pair is divisible by a given number k.
    * For example - {9,7,5,3}, k=6 => {{9,3},{7,5}}
    *
    * @param arr
    * @return true / false
    */
  def isArrayPairsDivisible(arr: Array[Int], k: Int): Boolean = {
    // Naive solution would be for every element in the array, look for a potential element that could partner to produce sum divisible by k
    // other solution - use hashing and count for every element depending on their mod k.
    // so in our example, we will use a count array (size k=6) that will look like {0,1,0,2,0,1}
    // now the idea is that for every element in the mod k array (iterate until arrModK.size /2), it can only partner up with (k-index)%k
    // in our example,
    // index 0 could partner up with index 0 to produce pair divided by k (0 with 0)
    // index 1 could partner up with index 5 to produce pair divided by k (1 with 1)
    // index 2 could partner up with index 4 to produce pair divided by k (0 with 0)
    // index 3 could partner up with index 3 to produce pair divided by k (2 with 2)
    val arrModk = scala.collection.mutable.ArrayBuffer.fill[Int](k)(0)
    arr.foreach { e => arrModk(e % k) += 1 }
    (0 until (k / 2) + 1).forall(i => arrModk(i) == arrModk((k-i)%k))
  }

  /**
    * Given a 2D matrix of booleans where true indicates white and false indicates black.
    * Considering group of adjacent black cells to be one blob, find the number of blobs in the matrix.
    * Example: consider the below 6x5 matrix:
    *     0	1	2	3	4
    *   0 T F T F T
    *   1 T F T F T
    *   2 T F F F T
    *   3 T T T T T
    *   4 F T T T F
    *   5	T T F F F
    * http://prismoskills.appspot.com/lessons/Arrays/Count_blobs_in_matrix.jsp
    * the solution function should output a count of 3 in the above case.
    *
    * @param matrix
    * @return number of blobs
    */
  def countBlobsIn2DMatrix(matrix: Array[Array[Boolean]], rows: Int, cols: Int): Int = {
    val visited = ArrayBuffer.tabulate(rows, cols)((x, y) => false)
    var blobsCount = 0

    def scanAdjacents(row: Int, col: Int): Unit = {
      // scan up, down, left, right
      if (row > 0 && row < rows && col > 0 && col < cols && !visited(row)(col)) {
        visited(row)(col) = true
        if(!matrix(row)(col)) {
          scanAdjacents(row + 1, col)
          scanAdjacents(row - 1, col)
          scanAdjacents(row, col + 1)
          scanAdjacents(row, col - 1)
        }
      }
    }

    for (i <- 0 until rows)
      for (j <- 0 until cols) {
        if (!visited(i)(j)) {
          if (!matrix(i)(j)) {
            // new blob{
            blobsCount += 1
            scanAdjacents(i, j)
          }
        }
      }
    blobsCount
  }

  /**
    * Given an array of unsorted integers, divide it into two sets, each having (arr.length/2) elements such that the sum of each set is as close to each other as possible.
    * Example -
    * Input : Arr[] = {4,5,3,1,5,2}
    * Output :  {5,4,1}, {5,3,2}
    *
    * @param arr
    * @return arrays divided according to requirements
    */
  def divideArraysToCloseSum(arr: Array[Int]): (Array[Int], Array[Int]) = {
    // this can be done by first sorting the array (O nlogn) and then applying the following algorithm:
    // maintain running sums for each set.
    // add current largest element into the set with smaller sum and update running sums.
    // if any set reaches capacity n/2, then simply put remaining elements into the other set.
    def divideArraysToCloseSumRec(arr1: Array[Int], arr2: Array[Int], sumArr1: Int, sumArr2: Int, arrSorted: Array[Int], index: Int): (Array[Int], Array[Int]) = {
      if (index < 0) arr1 -> arr2
      else {
        if (sumArr1 <= sumArr2) divideArraysToCloseSumRec(arr1 :+ arrSorted(index), arr2, sumArr1 + arrSorted(index), sumArr2, arrSorted, index + 1)
        else divideArraysToCloseSumRec(arr1, arr2 :+ arrSorted(index), sumArr1, sumArr2 + arrSorted(index), arrSorted, index + 1)
      }
    }
    divideArraysToCloseSumRec(Array.empty[Int], Array.empty[Int], 0, 0, arr.sortWith(_ > _), 0)
  }

  /**
    * A peak element is defined as an element which is NOT smaller than its immediate neighbors.
    * Two exceptions to a peak element are:
    * First element is a peak element if its not smaller than second element.
    * Last element is a peak element if its not smaller than second-last element.
    * For an array with all elements equal, all elements are peak elements.
    * Find a one pick element efficiently as possible in a given array
    * Example -
    * Peak elements in [5, 7, 3, 9, 10, 12] are 7 and 12
    *
    * @param arr
    * @return array includes all the peck elements from the original array
    */
  def findOnePeakEfficiently(arr: Array[Int]): Option[Int] = {
    // Simple solution would be to iterate via the array in O(n) time
    // however, this can be solved in log(n) time using binary search without sorting:
    // if an element is not the peak, then that element must be smaller than one of its neighbors.
    // if left neighbor is greater, then left half of the array must have a peak.
    // similarly, if right neighbor is greater, then right half of the array must have a peak.
    def findOnePeakEfficientlyRec(low: Int, high: Int): Option[Int] = {
      if(low > high) None
      else {
        val mid = low + (high - low)/2;  // same as (low + high)/2 but prevents overflow
        // first element check
        if(mid == 0 && arr(mid+1) <= arr(mid)) {
          Some(arr(mid))
        }
        // last element checked
        else if(mid == arr.size-1 && arr(mid-1) <= arr(mid)) {
          Some(arr(mid))
        }
        // else
        else {
          val leftSmaller  = arr(mid-1) <= arr(mid)
          val rightSmaller = arr(mid+1) <= arr(mid)
          if(leftSmaller && rightSmaller) Some(arr(mid))
          else {
            // If left neighbor is greater than current mid,
            // then peak must be present in the left half
           if(!leftSmaller) findOnePeakEfficientlyRec(low, mid-1)
           else findOnePeakEfficientlyRec(mid+1, high)
          }
        }
      }
    }
    findOnePeakEfficientlyRec(0, arr.size - 1)
  }

  /**
    * Given an unsorted array of non-negative integers, find all the subarrays whose sum is a given number K.
    *
    * @param arr
    */
  def findPairsSum(arr: Array[Int], k: Int): Unit = {
    def findPairsSumRec(arr: Array[Int], leftIndex: Int, rightIndex: Int): Unit = {
       if(leftIndex <= rightIndex) {
         val sum = arr(leftIndex) + arr(rightIndex)
         if(sum == k) {
           println(s"{${arr(leftIndex)},${arr(rightIndex)}}")
           findPairsSumRec(arr, leftIndex + 1, rightIndex - 1)
         } else if(sum > k) {
           findPairsSumRec(arr, leftIndex, rightIndex - 1)
         } else {
           findPairsSumRec(arr, leftIndex + 1, rightIndex)
         }
       }
    }
    // sort the array and scan the array using two indexes (beginning and end), advance or reduce indexes according to sum
    findPairsSumRec(arr.sorted, 0, arr.size - 1)
  }

  /**
    * Given an unsorted array of non-negative integers, find all the subarrays whose sum is a given number K in O(n) time
    * Example
    * arr={1,4,3,2,3,5,8}, k=8 => {1,4,3}, {3,2,3}, {3,5}, {8}
    *
    * @param arr
    */
  def findAllSubArraysWithAGivenSum(arr: Array[Int], k: Int): Unit = {
    def findAllSubArraysWithAGivenSumRec(startIndex: Int, forwardIndex: Int, currentSum: Int): Unit = {
      if(startIndex < arr.size && forwardIndex < arr.size) {
        if(currentSum == k) {
          val res = for(i <- startIndex to forwardIndex) yield arr(i)
          println(res.mkString(","))
          findAllSubArraysWithAGivenSumRec(startIndex+1, forwardIndex, currentSum - arr(startIndex))
        }
        else if(currentSum > k) findAllSubArraysWithAGivenSumRec(startIndex+1, forwardIndex, currentSum - arr(startIndex))
        else findAllSubArraysWithAGivenSumRec(startIndex, forwardIndex+1, currentSum + (if (forwardIndex + 1 == arr.size) 0 else arr(forwardIndex+1)))
      }
    }
    // keep on adding elements in current_sum till its less than the given sum.
    // if it becomes greater than given sum, start subtracting elements from the start of the array till its greater than given sum.
    findAllSubArraysWithAGivenSumRec(0, 0, arr(0))
  }

  /**
    * Given two arrays, find two elements (one in each array) such that their difference is minimum
    * Example:
    * Input : A[] = {l, 3, 15, 11, 2}
    *         B[] = {23, 127, 235, 19, 8}
    * Output : 3
    * That is, the pair (11, 8)
    *
    * @param arr1
    * @param arr2
    */
  def findAllSubArraysWithAGivenSum(arr1: Array[Int], arr2: Array[Int]): Int = {
    // O(n^2)
    def usingBruteForce: Int = {
      var minDiff = Integer.MAX_VALUE
      for(i <- 0 until arr1.size)
        for(j <- 0 until arr2.size) {
          val diff = Math.abs(arr1(i) - arr2(j))
          if(diff < minDiff) {
            minDiff = diff
          }
        }
      minDiff
    }

    // O(nlogn)
    def usingSorting(arr1Sorted: Array[Int], arr2Sorted: Array[Int], index1: Int, index2: Int, minDiff: Int): Int = {
      if(index1 >= arr1Sorted.size || index2 >= arr2Sorted.size) {
        minDiff
      }
      else {
        val minDiffUpdated = {
          val diff = Math.abs(arr1Sorted(index1) - arr2Sorted(index2))
          if(diff < minDiff) diff else minDiff
        }
        // move the array index of the smaller value
        if(arr1(index1) < arr2(index2)) usingSorting(arr1Sorted, arr2Sorted, index1+1, index2, minDiffUpdated)
        else usingSorting(arr1Sorted, arr2Sorted, index1, index2+1, minDiffUpdated)
      }
    }

    usingSorting(arr1.sorted, arr2.sorted, 0, 0, Integer.MAX_VALUE)
  }

  /**
    * Given an array in which all numbers except two are repeated once. (i.e. we have 2n+2 numbers and n numbers are occurring twice and remaining two have occurred once).
    * Find those two numbers in the most efficient way.
    *
    * @param arr
    * @return two repeated numbers
    */
  def findTwoNonRepeatedNumbers(arr: Array[Int]): (Int,Int) = {

    // hashing (complexity - o(n), space - o(n))
    def usingHashing: (Int,Int) = {
      val res = arr
                .groupBy(identity)
                .map { case (num,occurrencesArr) => num -> occurrencesArr.size }
                .collect { case (num, occurrences) if occurrences == 1 => num }
                .toArray
      (res(0), res(1))
    }

    // brute force - sorting (complexity - o(nlogn), space - o(1))
    def usingSorting(arrSorted: Array[Int], index: Int, nonRepeatedNumber1: Option[Int]): (Int,Int) = {
      if (index == arrSorted.length - 1) (nonRepeatedNumber1.get, arrSorted(index))
      else {
        if (arrSorted(index - 1) != arrSorted(index) && arrSorted(index + 1) != arrSorted(index)) {
          nonRepeatedNumber1.map((_, arrSorted(index))).getOrElse(usingSorting(arrSorted, index + 1, Some(arrSorted(index))))
        } else {
          usingSorting(arrSorted, index + 1, nonRepeatedNumber1)
        }
      }
    }

    // XOR - sorting (complexity - o(n), space - o(1))
    def usingXor(xorResult: Int): (Int, Int) = {
      // Explanation - If we have repeated pair of numbers, they would not add anything to xor results, as the xor of them would be zero. Only pair of different number would add non zero bits to xor result.
      // Now in c xor d, the only set bits are the bits that are different in c and d. Let's say 3rd bit is set in c xor d. This means if bit 3 is 0 in c it would be 1 in d or vice versa.
      // So if we divide all numbers in 2 group, one which contains all numbers with bit 3 is 0, and other in which bit 3 is 1, c and d would definitely go to different groups.
      // And all pairs of same numbers would go the same group. (Bit 3 is either 1 on both a or 0 in both a)

      //(n – 1) will have all the bits flipped after the rightmost set bit of n (1100 -> 1011, 1010 -> 1001)
      val bitTurnedOnInXorResult = xorResult & ~(xorResult-1) // bit formula to get the first bit turned on from the right (not the index but the value!, so for 0100 -> the result will be 4)
      arr.foldLeft((0, 0)) { case ((num1,num2), element) =>
        if((element & bitTurnedOnInXorResult) == 0) (num1 ^ element,num2) else (num1,num2 ^ element)
      }
    }

    //usingHashing
    //usingSorting(arr.sorted, 1, None)
    usingXor(arr.reduceLeft(_ ^ _))
  }

  /**
    * Find kthmaximum in an unsorted array
    *
    * @param arr
    * @param k
    * @return kthmaximum
    */
  def kthMaximum(arr: Array[Int], k: Int): Int = {
    require(k > 0 && k <= arr.size)

    // complexity - O(nk)
    def usingVariationOfBubbleSort: Int = {
      //Find maximum element in the array and swap it with the end of the array.
      //then find maximum in N-1 array elements. After k iterations, element at N-k position will be the kth largest.
      for(i <- 0 until k) {
        val maximumElement = arr.take(arr.size - i).zipWithIndex.maxBy { case (number,index) => number }._2 // find the index of the largest element
        // put the biggest element in the last of the array (array from 0 to arr.size - k)
        val temp = arr(arr.size - 1 - i)
        arr(arr.size - 1 - i) = arr(maximumElement)
        arr(maximumElement) = temp
      }

      // return the k element from the end of the array which is the kth biggest element in the array
      arr(arr.size - k)
    }

    // 1) Build a Max Heap tree in O(n)
    // 2) Use Extract Max k times to get k maximum elements from the Max Heap O(klogn)
    def usingHeapSort = {
      val pq = collection.mutable.PriorityQueue(arr: _*)
      def dequeue(counter: Int): Int = {
        if(counter == 1) pq.dequeue()
        else {
          pq.dequeue()
          dequeue(counter - 1)
        }
      }
      dequeue(k)
    }

    // 1) Build a Min Heap MH of the first k elements (arr[0] to arr[k-1]) of the given array. O(k)
    // 2) For each element, after the kth element (arr[k] to arr[n-1]), compare it with root of MH.
    //  ……a) If the element is greater than the root then make it root and call heapify for MH
    //  ……b) Else ignore it.
    // The step 2 is O((n-k)*logk)
    // 3) Finally, MH has k largest elements and root of the MH is the kth largest element.
    def usingMinHeapSizeK = {
      val pq = collection.mutable.PriorityQueue(arr.take(k): _*)(new Ordering[Int] {
        def compare(x:Int, y:Int) = y compare x
      }) // put first k elements in the min heap

      val asdsa = pq.head
      for(i <- k until arr.size) {
        val root = pq.max
        if(root < arr(i)) {
          pq.dequeue
          pq.enqueue(arr(i))
        }
      }

      pq.head
    }

    //usingVariationOfBubbleSort
    //usingHeapSort
    usingMinHeapSizeK
  }

  /**
    * Find largest bitonic (first increasing, then decreasing) sub-array
    * Example: If given array is {2, 5 10, 8, 12, 15, 7, 3, 0}
    * The in the above array, there are two bitonics - {2,5,10, 8} and {8, 12, 15, 7, 3, 0}
    * {20, 4, 1, 2, 3, 4, 2, 10} => {1, 2, 3, 4, 2}
    * {12, 4, 78, 90, 45, 23} => {4, 78, 90, 45, 23}
    * {10} => {10}
    * {10, 20, 30, 40} => {10, 20, 30, 40}
    * {40, 30, 20, 10} => {40, 30, 20, 10}
    *
    * @param arr
    * @return kthmaximum
    */
  def largestBitonic(arr: Array[Int]): Array[Int] = {

    def determineLargestBitonic(bitonicsIndexes: Array[Int]): Array[Int] = {
      var biggestBitonic: IndexedSeq[Int] = IndexedSeq()
      bitonicsIndexes.foreach { bitonicIndex => // for every bitonic peak index, create array of the entire bitonic and save it if its the biggest
        val leftSideBitonic  = (bitonicIndex - 1 to 0 by -1).takeWhile(i => arr(i) < arr(i+1)).reverse.map(arr(_)) // scan left
        val rightSideBitonic = (bitonicIndex + 1 until arr.size).takeWhile(i => arr(i) < arr(i-1)).map(arr(_))     // scan right
        val bitonicFinal = (leftSideBitonic :+ arr(bitonicIndex)) ++ rightSideBitonic // concat left side + bitonic peak + right side
        if(bitonicFinal.size > biggestBitonic.size) {
          biggestBitonic = bitonicFinal
        }
      }
      biggestBitonic.toArray
    }

    val bitonicsIndexes = ArrayBuffer.empty[Int]
    for(i <- 1 until arr.size - 1) {
      if(arr(i -1) < arr(i) && arr(i) > arr(i + 1))  bitonicsIndexes += i
    }

    if(bitonicsIndexes.isEmpty) arr else determineLargestBitonic(bitonicsIndexes.toArray)
  }

  /**
    * Largest rectangle in a histogram
    * For simplicity, assume that all bars have same width and the width is 1 unit
    * Example - {6, 2, 5, 4, 5, 1, 6}, The largest possible rectangle possible is 12
    * https://www.geeksforgeeks.org/largest-rectangular-area-in-a-histogram-set-1/
    *  _
    * | |
    * | |_
    * |   |
    * |   |_
    * |     |
    *
    * @param arr
    * @return largest rectangle in a histogram
    */
  def largestRectangleOnAHistogram(arr: Array[Int]): Int = {
    // Brute force - for every element in the array, calculate its max rectangle (complexity O(n^2))
    // there are better solutions in terms of complexity that can be found online
    var min, max = arr(0)
    for(i <- 0 until arr.size) {
      min = arr(i)
      for(j <- i + 1 until arr.size) {
        min = Math.min(min, arr(j))
        max = Math.max(max, min * (j - i + 1))
      }
    }
    max
  }

  /**
    * Given a boolean array with 1s and 0s, find the largest subarray with equal number of 1s and 0s.
    * Examples:
    * Input: arr[] = {1, 0, 1, 1, 1, 0, 0}, Output: 1 to 6 (Starting and Ending indexes of output subarray)
    * Input: arr[] = {1, 1, 1, 1}, Output: No such subarray
    * Input: arr[] = {0, 0, 1, 1, 0}, Output: 0 to 3 Or 1 to 4
    *
    * @param arr
    * @return largest subarray with equal number of 1s and 0s
    */
  def largestSubArrayWithEqualNumberOfOnesAndZeros(arr: Array[Int]): (Int,Int) = {
    def bruteForce(arr: Array[Int]) = {
      var numberOfZeros    = 0
      var numberOfOnes     = 0
      var maxSubArrayIndex = 0 -> 0
      for(i <- 0 until arr.size) {
        numberOfZeros = 0
        numberOfOnes = 0
        for (j <- i until arr.size) {
          if(arr(j) == 0) numberOfZeros+=1 else numberOfOnes+=1
          if(numberOfZeros == numberOfOnes) {
            if(j - i > maxSubArrayIndex._2 - maxSubArrayIndex._1) {
              maxSubArrayIndex = i -> j
            }
          }
        }
      }
      maxSubArrayIndex
    }

    //this can be done using O(n) complexity - see https://www.geeksforgeeks.org/largest-subarray-with-equal-number-of-0s-and-1s/

    bruteForce(arr)
  }

  /**
    * Check for Majority Element in a sorted array (majority element is an element that appears more then n/2 times, so there can be only one, if any)
    * Examples:
    * Input: arr[] = {1, 2, 3, 3, 3, 3, 10   , Output: Some(3)
    * Input: arr[] = {1, 1, 2, 4, 4, 4, 6, 6}, Output: None
    * Input: arr[] = {1, 1, 1, 2, 2}         , Output: Some(1)
    *
    * @param arr
    * @return largest subarray with equal number of 1s and 0s
    */
  def majorityElementInASortedArray(arr: Array[Int]): Option[Int] = {

    def helper(firstOccurrence: Int, x: Int) = {
      val arrSize = arr.size
      Option(firstOccurrence)
        .filter(_ != -1) // found index
        .filter(_ < (if(arrSize % 2 == 0) arrSize / 2 else arrSize / 2 + 1)) // index is smaller then half the size of the array
        .collect { case firstOccurrenceIndex if(arr(firstOccurrenceIndex + arr.size / 2) == x) => x }
    }

    def linearSearch(x: Int): Option[Int] = {
      //Linearly search for the first occurrence of the element, once you find it (let at index i), check element at index i + n/2. If element is present at i+n/2 then return 1 else return 0.
      helper(arr.indexOf(x), x)
    }

    def binarySearch(x: Int) = {
      //Use binary search methodology to find the first occurrence of the given number
      def findFirstOccurrence(left: Int, right: Int): Int = {
        if(left > right) -1
        else {
          val mid = left + (right - left) / 2
          if(arr(mid) == x && (mid == 0 || arr(mid-1) < x)) mid // if arr(mid) equals to x AND (the element to my left is smaller then x then I'm the first occurrence OR i'm in position zero index)
          else if(arr(mid) < x) findFirstOccurrence(mid + 1, right)
          else findFirstOccurrence(left, mid - 1)
        }
      }
      helper(findFirstOccurrence(0, arr.size - 1), x)
    }

    val majorityCandidate = arr(arr.size / 2)
    binarySearch(majorityCandidate)
  }

  /**
    * Given two sorted arrays, find a path such that it uses elements from either of the arrays at a time and sum of whose elements is maximum.
    * Only condition to satisfy is that you can change from one array to another only when the elements are matching.
    * Examples:
    * arr1 = [1, 2, 8, 9, 10, 15, 20]
    * arr2 = [7, 8, 10, 11, 20, 25]
    * Then maximum sum path = 7, 8, 9, 10, 15, 20, 25
    * Some other paths possible are: 1,2,8,10,11,20 or 7,8,10,15,20
    * But since the sum of their elements is not maximum, they do not form the answer.
    *
    * @param arr1
    * @param arr2
    * @return largest subarray with equal number of 1s and 0s
    */
  def maximumPathSortedArrays(arr1: Array[Int], arr2: Array[Int]): Int = {
    def maximumPathSortedArraysRec(index1: Int, index2: Int, sum1: Int, sum2: Int, result: Int): Int = {
      if(index1 >= arr1.size) {
        result + arr2.drop(index2).reduce(_ + _) //takes what is left from arr2 and add it to sum
      }
      else if(index2 >= arr2.size) {
        result + arr1.drop(index1).reduce(_ + _) //takes what is left from arr1 and add it to sum
      }
      else if(arr1(index1) < arr2(index2)) maximumPathSortedArraysRec(index1 + 1, index2, sum1 + arr1(index1), sum2, result) //advance index1 and add to sum1
      else if(arr1(index1) > arr2(index2)) maximumPathSortedArraysRec(index1, index2 + 1, sum1, sum2 + arr2(index2), result) //advance index2 and add to sum2
      else maximumPathSortedArraysRec(index1 + 1, index2 + 1, 0, 0, result + (if(sum1 > sum2) sum1 else sum2) + arr1(index1)) //we encounter same element - compare sum until now from last encounter element and add it to the result along with the encounter element
    }
    maximumPathSortedArraysRec(0, 0, 0, 0, 0)
  }

  /**
    * There are 2 sorted arrays A and B of size n each. Write an algorithm to find the median of the array obtained after merging the above 2 arrays(i.e. array of length 2n)
    * Example:
    * arr1 = [1, 12, 15, 26, 38]
    * arr2 = [2, 13, 17, 30, 45]
    * after merging the array [1,2,12,13,15,17,26,30,38,45] middle two elements are 15 and 17. average of middle elements is (15+17)/2 = 16
    *
    * @param arr1
    * @param arr2
    * @return median of two sorted arrays
    */
  def medianOfTwoSortedArray(arr1: Array[Int], arr2: Array[Int]): Int = {
    var m1, m2 = 0
    var i,j    = 0
    val size = arr1.size

    for(count <- 0 to size) {
      /* Below is to handle case where all
         elements of ar1[] are smaller than
         smallest(or first) element of ar2[] */
      if (i == size) {
        m1 = m2
        m2 = arr2(0)
      }

      /* Below is to handle case where all
         elements of ar2[] are smaller than
         smallest(or first) element of ar1[] */
      else if (j == size) {
        m1 = m2
        m2 = arr2(0)
      }

      else {
        if (arr1(i) < arr2(j)) {
          m1 = m2
          m2 = arr1(i)
          i += 1
        }

        else {
          m1 = m2
          m2 = arr2(j)
          j += 1
        }
      }
    }
    (m1 + m2) / 2
  }

  /**
    * Given an unsorted array, print the nearest larger element for each element.
    * The Next greater Element for an element x is the first greater element on the right side of x in array. Elements for which no greater element exist, consider next greater element as None
    * Example:
    * Given array [5, 9, 3, 5, 4]
    * The solution should print:
    * 5 --nearest largest element-- > 9
    * 9 --nearest largest element-- > None
    * 3 --nearest largest element-- > 5 (NOT 4)
    * 5 --nearest largest element-- > None
    * 4 --nearest largest element-- > None
    *
    * @param arr
    */
  def NearestLargerElementForEachElement(arr: Array[Int]): Unit = {
    //O(n) solution
    def usingTwoLoops = {
      var next: Option[Int] = None
      for(i <- 0 until arr.size) {
        next = None
        for(j <- i+1 until arr.size if(next.isEmpty)) {
          if(arr(i) < arr(j)) {
            next = Some(arr(j))
          }
        }
        print(arr(i) -> next)
      }
    }

    //O(n) solution - https://www.geeksforgeeks.org/next-greater-element/
    def usingDecreasingStack = {
      val myStack = new MyStack[Int]
      myStack.push(arr(0))
      for(i <- 1 until arr.size) {
        val next = arr(i)
        // print all elements in stack that are smaller then next
        while(!myStack.isEmpty && myStack.peek < next) {
          print(myStack.pop -> next)
        }
        // current element's next larger element is yet to be seen, so current element always goes into the stack.
        myStack.push(next)
      }
      // The elements left in the stack have no next larger element.
      // Infact, the leftovers are arranged in sorted order in the stack due to this.
      while(!myStack.isEmpty) print(myStack.pop -> None)
    }
  }

  /**
    * Search an element in a 2D array (matrix) sorted row-wise and col-wise
    * Example:
    *  01   06    09
    *  03   08    12
    *  05   10    14
    *
    * @param arr
    * @param element
    */
  def SearchElementInSorted2DMatrix(arr: Array[Array[Int]], element: Int): Unit = {
    var i = 0
    var j = arr.size - 1
    while(i < arr.size && j >= 0) {
      if(arr(i)(j) == element) print(s"found element! - ${element}")
      else if(arr(i)(j) < element) i+=1
      else j+=1
    }
    //This solution is guaranteed to work because at every step we are eliminating a full row or columns which is less than or greater than the given element.
  }

  /**
    * Given an unsorted array, find two elements a[i] and a[j] such that i < j and difference between a[i] and a[j] is maximum in the array.
    * Example:
    * in the following array [30, 12, 15, 22, 25, 7, 18], the elements with maximum difference are 12 and 25
    *
    * @param arr
    */
  def maxDifferenceBetweenTwoElementsInAnArray(arr: Array[Int]): (Int,Int) = {
    // O(n^2) solution
    def naiveSolution: (Int,Int) = {
      var maxDifference = Integer.MIN_VALUE
      var elem1, elem2 = 0
      for(i <- 0 until arr.size)
        for(j <- i+1 until arr.size) {
          if(arr(j) - arr(i) > maxDifference) {
            elem1 = arr(i)
            elem2 = arr(j)
            maxDifference = arr(j) - arr(i)
          }
        }
      elem1 -> elem2
    }

    // O(n) solution
    def keepTrackOfMinimumElementSolution = {
      // In this method, instead of taking difference of the picked element with every other element,
      // we take the difference with the minimum element found so far. So we need to keep track of 2 things:
      // 1) Maximum difference found so far (max_diff).
      // 2) Minimum number visited so far (min_element).
      var minimum = arr(0)
      var elem1 = arr(0)
      var elem2 = arr(1)
      var maxDifference = arr(1) - arr(0)
      for(i <- 1 until arr.size) {
        if(arr(i) - minimum > maxDifference) {
          maxDifference = arr(i) - minimum
          elem1 = minimum
          elem2 = arr(i)
        }
        if(arr(i) < minimum) {
          minimum = arr(i)
        }
      }
      elem1 -> elem2
    }

    keepTrackOfMinimumElementSolution
  }

  /**
    * Given an infinite supply of ‘m’ coin denominations S[m] = {S1, S2... Sm}, calculate all the different combinations which can be used to get change for some quantity ‘N’
    * So For:
    * if N = 4 and S = {1,2,3}, then different ways possible are
    * {1,1,1,1}, {2,1,1}, {3,1}, {2,2}
    *
    * @param coins
    * @param sum
    */
  def allCombinationsToSumCoins(coins: Array[Int], sum: Int): Int = {
    if(sum == 0) 1
    else if(sum < 0 || coins.isEmpty) 0
    else {
      val withCoin = allCombinationsToSumCoins(coins, sum - coins(0))
      val withoutCoin = allCombinationsToSumCoins(coins.tail, sum)
      withCoin + withoutCoin
    }
  }

  /**
    * Given a string - return a set of all of its permutations
    * So For:
    * str="abc" => {"abc","acb","bac","bca","cab","cba"}
    *
    * @param str
    * @return Set[String]
    */
  def allPermutationsToSet(str: String): Set[String] = {
    val set = new ArrayBuffer[String]()
    def allPermutationsToSetRec(str: String, prefix: String): Unit = {
      if(str == "") set += prefix
      else {
        for(i <- 0 until str.length) {
          // a+bc, b+ac, c+ab ...
          allPermutationsToSetRec(str.take(i) + str.drop(i + 1), prefix + str(i))
        }
      }
    }
    allPermutationsToSetRec(str, "")
    set.result().toSet
  }

  /**
    * Given a positive integer n, print all combination of numbers from 1 to n having sum n
    * For example, for n=5:
    * {5}
    * {1,4}
    * {2,3}
    * {1,1,3}
    * {1,2,2}
    * {1,1,1,2}
    * {1,1,1,1,1}
    *
    * @param number
    */
  def allCombinationsOfNumbersFromOneToN(number: Int): Set[String] = {
    val set = new ArrayBuffer[String]()

    def allCombinationsOfNumbersFromOneToNRec(numberStr: String, index: Int, sum: Int): Unit = {

      if(sum == 0) {
        set += numberStr
      }

      if(sum > 0) {
        for(i <- index to sum) {
          allCombinationsOfNumbersFromOneToNRec(numberStr + i.toString, i, sum - i)
        }
      }
    }
    // consider every integer from 1 to number and add it in the output and recurse for remaining elements [i..number] with reduced sum (n-i)
    allCombinationsOfNumbersFromOneToNRec("", 1, number)
    set.result().toSet
  }

  /**
    * Given an array of distinct integers, replace each element of the array by its corresponding rank in the array
    * the minimum element in the array has rank 1, the second minimum element has rank 2 and so on..
    * For example:
    * input={10,8,15,12,6,20,1}
    * output={4,3,6,5,2,7,1}
    *
    * @param arr
    */
  def replaceArrayElementsByRank(arr: Array[Int]): Unit = {

    // create ranks map
    val ranks = arr
      .sorted
      .zipWithIndex
      .toMap

    // replace each elemnt by its corresponding rank
    for(i <- 0 until arr.size) {
      arr(i) = ranks(arr(i)) + 1
    }
  }

  /**
    * print all possible triplet sets from a given array (size >= 3, no repetition and order doesn't count, i.e. - combinatorial)
    * For example:
    * arr={1,2,3,4}
    * results={1, 2, 3},{1, 2, 4},{1, 3, 4},{2, 3, 4}
    *
    * @param arr
    */
  def printAllPossibleTripletSets(arr: Array[Int]): Unit = {
    require(arr.size >= 3)

    val triplets = scala.collection.mutable.Set.empty[Set[Int]]

    def printAllPossibleTripletSetsRec(set: Set[Int], index: Int): Unit = {
      if(set.size == 3) {
        if(!triplets.contains(set)) {
          print(set)
          triplets += set
        }
      } else if(index < arr.size) {
        for(i <- index until arr.size) {
          printAllPossibleTripletSetsRec(set + arr(i), index + 1)
        }
      }
    }
    printAllPossibleTripletSetsRec(Set.empty, 0)
  }

  /**
    * Given a c-style string and a pattern, in-place replace all non-overlapping occurences of the pattern in the string by a specified character
    * For example:
    * string="ABCABCXABC", pattern="ABC", character='@'
    * result="@@X@"
    *
    * @param arr
    */
  def replaceInMemory(arr: Array[Char], pattern: Array[Char], character: Char): Unit = {

    // receive index of the array and check if the characters from index follow the given patter
    def isPatternMatched(index: Int): Boolean = {
      if(arr.size - index >= pattern.size) {
        arr.slice(index,pattern.size + index).deep == pattern.deep
      } else {
        false
      }
    }

    def replaceInMemoryRec(index: Int, position: Int): Unit = {
      if(index >= arr.size) {
        // padding '\n' until the end of the array
        if(position < arr.size) for (i <- position until arr.size) arr(i) = '\n'
      } else {
        // if pattern matched array index forward, fill character in the position index and increase index by pattern.size
        if(isPatternMatched(index)) {
          arr(position) = character
          replaceInMemoryRec(index + pattern.size, position + 1)
        } else {
          // else copy character into array in position index and advance index and position
          arr(position) = arr(index)
          replaceInMemoryRec(index + 1, position + 1)
        }
      }
    }

    replaceInMemoryRec(0, 0)
  }

  /**
    * Given an expression consisting of opening brace '{' and a closing brace '}', find the minimum number of inversions needed to make the expression balanced, -1 if it can't be balances.
    * For example:
    * expression="{{}{{}{{"
    * output=2, {{}{{}{{ -> {{}{{}}{ -> {{}{{}}}
    *
    * @param expression
    */
  def minimumNumberOfInversionsNeededForBalance(expression: String): Int = {
    // the idea is to traverse the given expression and maintain a count of open braces in the expression seen so far. if the current character is opening brace '{', then we increment the count of opened braces by 1. if the current character
    // is closing brace '}', then we check if it has any unclosed brace to its left (look for non zero opened brace count). if any unclosed brace is found, we close it by using current brace and decrement the count of opened braces by one;
    // else we convert the current closing brace '}' to '{' and increment the total inversions needed any opening brace count by 1.
    // in the end, if there are still opened braces, if the number of opend braces is even -> return exactly / 2, else the expression can't be balanced.
    def minimumNumberOfInversionsNeededForBalanceRec(index: Int, countOpen: Int, numberOfInversions: Int): Int = {
      if(index == expression.size) if(countOpen % 2 == 0) countOpen / 2 else -1
      else {
        expression.charAt(index) match {
          case '{' => minimumNumberOfInversionsNeededForBalanceRec(index + 1, countOpen + 1, numberOfInversions)
          case '}' => {
            if(countOpen > 0) minimumNumberOfInversionsNeededForBalanceRec(index + 1, countOpen = countOpen - 1, numberOfInversions) else minimumNumberOfInversionsNeededForBalanceRec(index + 1, countOpen = countOpen + 1, numberOfInversions + 1)
          }
        }
      }
    }

    minimumNumberOfInversionsNeededForBalanceRec(0,0,0)
  }

  /**
    * Given a 2D array of 1's and 0's, find the size of the largest block of 1's. For example the following 2D array:
    * int[][] array = {
    * {1, 0, 1, 0, 0, 0, 1, 0}
    * {1, 0, 0, 0, 0, 0, 1, 1}
    * {1, 1, 1, 0, 0, 0, 1, 1}
    * }
    *
    * the idea is as follows:
    *
    * 1	0	1	0	0	0	1	1
    * 1	0	0	0	0	0	0	0
    * 1	0	0	0	0	0	0	0
    *
    * 1	0	1	0	0	0	1	0
    * 1	0	0	0	0	0	1	1
    * 1	1	1	0	0	0	1	2
    *
    * Would return "4", because there is a 2x2 square of 1's, and that is the biggest block of 1's in the 2D space.
    *
    * @param mat
    */
  def largestBlockOfOnes(mat: Array[Array[Int]]): Int = {

    val arrayTemp = Array.ofDim[Int](mat.length, mat(0).length)

    // fill first col
    for(i <- 0 until mat.length) arrayTemp(i)(0) = mat(i)(0)

    // fill first col
    for(j <- 0 until mat.head.length) arrayTemp(0)(j) = mat(0)(j)

    var max = 0

    for(i <- 1 until  mat.length)
      for(j <- 1 until mat.head.length) {
        val min = Math.min(Math.min(arrayTemp(i)(j-1), arrayTemp(i-1)(j)), arrayTemp(i-1)(j-1)) + 1

        if(mat(i)(j) == 1) {
          val min = Math.min(Math.min(arrayTemp(i)(j-1), arrayTemp(i-1)(j)), arrayTemp(i-1)(j-1)) + 1
          arrayTemp(i)(j) = min
          if(min > max) {
            max = min
          }
        } else {
          mat(i)(j) = 0
        }
      }
    printMatrix(arrayTemp)
    max
  }

  /**
    * Given a 2D array of 1's and 0's, find the size of the largest block of 0's. For example the following 2D array:
    * int[][] array = {
    * {1, 0, 1, 0, 0, 0, 1, 0}
    * {1, 0, 0, 0, 0, 0, 1, 1}
    * {1, 1, 1, 0, 0, 0, 1, 1}
    * }
    * Would return "9", because there is a 3x3 square of 0's, and that is the biggest block of 0's in the 2D space.
    *
    * @param mat
    */
  def largestBlockOfZeros(mat: Array[Array[Int]]): Int = {

    val arrayTemp = Array.ofDim[Int](mat.length, mat(0).length)

    // fill first col
    for(i <- 0 until mat.length) arrayTemp(i)(0) = mat(i)(0) ^ 1

    // fill first col
    for(j <- 0 until mat.head.length) arrayTemp(0)(j) = mat(0)(j) ^ 1

    var max = 0

    for(i <- 1 until  mat.length)
      for(j <- 1 until mat.head.length) {

        if(mat(i)(j) == 0) {
          val min = Math.min(Math.min(arrayTemp(i)(j-1), arrayTemp(i-1)(j)), arrayTemp(i-1)(j-1)) + 1
          arrayTemp(i)(j) = min
          if(min > max) {
            max = min
          }
        } else {
          mat(i)(j) = 0
        }
      }

    max
  }

  /**
    * Given a list of jobs where each job has a start and finish time, and also has profit associated with it,
    * find maximum profit subset of non-overlapping jobs.
    *
    * For example, consider jobs with their starting time, finishing time, and associated profit.
    * Job1: (0,6,60)
    * Job1: (1,4,30)
    * Job1: (3,5,10)
    * Job1: (5,7,30)
    * Job1: (5,9,50)
    * Job1: (7,8,10)
    *
    * the maximum profit is 80 which is achived by picking job 2 and job 5
    *
    */
  object weightedIntervalSchedulingProblem {
    /** 1) First sort jobs according to finish time.
        2) Now apply following recursive process.
          // Here arr[] is array of n jobs
          findMaximumProfit(arr[], n)
        {
          a) if (n == 1) return arr[0];
          b) Return the maximum of following two profits.
          (i) Maximum profit by excluding current job, i.e.,
          findMaximumProfit(arr, n-1)
          (ii) Maximum profit by including the current

      The idea is to find the latest job before the current job (in
      sorted array) that doesn't conflict with current job 'arr[n-1]'.
      Once we find such a job, we recur for all jobs till that job and
      add profit of current job to result.
    } **/

    case class Job(start: Int, end: Int, profit: Int)

    def findLatestJobNonOverlapping(jobsList: Array[Job], index: Int, start: Int): Int = {
      index match {
        case num if num < 0                     => -1
        case num if jobsList(num).end <= start  => index
        case _                                  => findLatestJobNonOverlapping(jobsList, index - 1, start)
      }
    }

    def go(jobsList: Array[Job], index: Int): Int = {
      index match {
        case num if num < 0  => 0
        case num if num == 0 => jobsList(0).profit
        case _               => {
          // include current job
          val include = jobsList(index).profit + go(jobsList, findLatestJobNonOverlapping(jobsList, index - 1, jobsList(index).start))

          // exclude current job
          val exclude = go(jobsList, index - 1)

          // return max
          Math.max(include, exclude)
        }
      }
    }

    val job1 = Job(0,6,60)
    val job2 = Job(1,4,30)
    val job3 = Job(3,5,10)
    val job4 = Job(5,7,30)
    val job5 = Job(5,9,50)
    val job6 = Job(7,8,10)
    val arr = Array(job1, job2, job3, job4, job5, job6)
    print(go(arr.sortWith { case (j1,j2) => j1.end < j2.end }, arr.length - 1))
  }

  /**
    * Given an array of integers, rearrange the array such that it contains positive and negative numbers at alternate position.
    * If array contains more positive or negative elements, they should be moved to end of the array.
    * Examples:
    * Input: {9,-3,5,-2,-8,-6,1,3}
    * Output: {5,-2,9,-6,1,-8,3,-3}
    *
    * Input: {9,-3,5,-2,-8,-6}
    * Output: {5,-2,9,-6,-3,-8}
    *
    * Input: {9,-3,5,-2,8,6,1,3}
    * Output: {5,-2,9,-3,8,6,1,3}
    *
    * @param arr
    */
  def rearrangeArrayAlternateOrder(arr: Array[Int]): Unit = {

    /** the idea is to use 0 as pivot element and make one pass of partition process. the resultant array will contain
    all positive integers at the end of the array and all negative integers in the beginning. then we swap alternat negative
    element from next available positive element till end of array is reached or all negative or positive integers are exhausted. **/

    def pivot(arr: Array[Int]): Int = {
      val pivot = 0
      var j = 0
      for (i <- 0 until arr.length) {
        if (arr(i) < pivot) {
          val temp = arr(j)
          arr(j) = arr(i)
          arr(i) = temp
          j += 1
        }
      }
      j
    }

    var firstPositiveIndex: Int = pivot(arr)
    for (i <- 0 until arr.length by 2 if firstPositiveIndex < arr.length && i < firstPositiveIndex) {
      val temp = arr(i)
      arr(i) = arr(firstPositiveIndex)
      arr(firstPositiveIndex) = temp
      firstPositiveIndex += 1
    }
  }

  /**
    * Given an unsorted array of integers whoch each element lies in range 0 to n-1 where n is the size of the array,
    * calculate the frequency of all elements presnet in the array in linear time and using constan space.
    * Example:
    * Input: {2,3,3,2,1}
    * Output: Element 1 appears 1 times, Element 2 appears 2 times, Element 3 appears 2 times
    *
    * @param arr
    */
  def frequencyOfAllElementsPresentInTheArrayLinearTime(arr: Array[Int]): Unit = {

    def solutionWithConstanSpace = {
      val arrBuffer = Array.fill(arr.length){0}
      arr.foreach(arrBuffer(_) += 1)
      for(i <- 0 until arrBuffer.length) {
        if(arrBuffer(i) > 0) {
          println(s"Element ${i} appears ${arrBuffer(i)} times")
        }
      }
    }

    /** We can solve this problem using any extra space by taking advantage of the fact that array elements lies in the range 0 to n-1.
        For each element A[i] present in the array, we increment value present at index (A[i] % n) by n.
        Finally, we traverse the modified array and if A[i] is more than or equal to n, then i appears in the array (A[i]/n) times.
        For example, consider the array {2,3,3,2,1}. After incrementing value present at index (A[i] %n) for each element A[i] by n,
        the array becomes {2,8,13,12,1}. Now if we take (arr[i]/n) for each index i, we get {0,1,2,2,0}. Here, A[i] denotes the frequency of index i.
    **/
    def `solutionWith(O1)Space` = {
      val arrLength = arr.length
      for(i <- 0 until arrLength) {
        arr(arr(i) % arrLength) += arrLength
      }

      for(i <- 0 until arrLength) {
        if(arr(i) >= arrLength) {
          println(s"Element ${i} appears ${arr(i)/arrLength} times")
        }
      }
    }
  }

  /**
    * Given a positive integer n, print all combination of numbers from 1 to n having sum n
    * For Example:
    * For n=5, below combinations are possible:
    * 5
    * 1,4
    * 2,3
    * 1,1,3
    * 1,2,2
    * 1,1,1,2
    * 1,1,1,1,1
    *
    * @param n
    */
  def printAllCombinationOfNumbersFrom1ToNHavingSumN(n: Int): Unit = {

    // utility function to print only sorted arrays (to prevent reoccurring of unsorted combinations)
    def checkSortedArray(arr: Array[Int], index: Int = 0): Boolean = {
      if(index == arr.length || arr.length <= 1) {
        true
      } else if(index == 0 || arr(index - 1) <= arr(index)) {
        checkSortedArray(arr, index + 1)
      } else {
        false
      }
    }

    def printAllCombinationOfNumbersFrom1ToNHavingSumNRecAttempt1(arr: Array[Int]): Unit = {
      if(arr.sum == n && checkSortedArray(arr)) {
        println(arr.mkString(","))
      }

      else if(arr.sum < n) {
        for(i <- 1 until n) {
          printAllCombinationOfNumbersFrom1ToNHavingSumNRecAttempt1(arr :+ i)
        }
      }
    }

    println(n)
    printAllCombinationOfNumbersFrom1ToNHavingSumNRecAttempt1(Array.empty[Int])
  }

  /**
    * Given an array of distinct integers, replace each element of the array by its corresponding rank in the array.
    * The minimum element in the array has rank 1, the second minimum element has rank 2 and so on.
    * For Example:
    * Input: {10,8,15,12,6,20,1}
    * Output: {4,3,6,5,2,7,1}
    *
    * @param arr
    */
  def ArrayCorrespondingRank(arr: Array[Int]): Unit = {
    val map = arr.sorted.zipWithIndex.toMap
    print(arr.map(map(_) + 1).mkString(","))
  }
 
 /**
    * Print all triplets in an array with sum equal to given number
    * For Example:
    * A=[2,7,4,9,5,1,3]
    * sum=10
    *
    * Possible output: (1,2,3), (1,2,4), (2,3,4), ...
    *
    * @param arr
    */
  def printAllTripletsMatchAGivenSum(arr: Array[Int], sum: Int): Unit = {
    // this is one version that goes over all the permutations but its less efficient
    def go(buff: Array[Int], index: Int): Unit = {
      if(buff.length == 3 && buff.sum == sum) {
        println(buff.mkString(","))
      } else if(index < arr.length && buff.size <= 2) {
        for(i <- index until arr.length) {
          go(buff :+ arr(i), i + 1)
        }
      }
    }

    // for each element arr(i), check if triplets can be formed by arr(i) and pairs from sub-array[i+1,..,n]
    def goEfficient() = {
      for (i <- 0 until arr.length - 3) {
        var low = i + 1
        var high = arr.length - 1
        // loop till low is less than high
        while(low < high) {
          val currentSum = arr(i) + arr(low) + arr(high)
          if(currentSum == sum) {
            println(s"${arr(i)},${arr(low)},${arr(high)}")
            low += 1
            high -= 1
          }
          if(arr(i) + arr(low) + arr(high) > sum) {
            // decrement high if total is more than the remaining sum
            high -= 1
          } else {
            // increment low if total is less than the remaining sum
            low += 1
          }
        }
      }
    }

    //go(Array.empty[Int], 0)
    goEfficient
  }
 
   /**
    * Given an unsorted array of integers containing many duplicates elements, rearrange the given array such that same element appears
    * together and relative order of rist occurrence of each element remains unchanged
    * For Example:
    * Input={1,2,3,1,2,1}
    * Output={1,1,1,2,2,3}
    * Input={5,4,5,5,3,1,2,2,4}
    * Output={5,5,5,4,4,3,1,2,2}
    *
    * @param arr
    */
  def groupElementsOfAnArrayBasedOnTheirFirstOccurrence(arr: Array[Int]): Array[Int] = {
    val map = arr.groupBy(identity).mapValues(_.length)
    arr.map(num => num -> map(num))
       .distinct
       .flatMap { case (e,c) => for(i <- 0 until c) yield e }
  }

  /**
    * Given two integers, find minimum difference between their index in a given array in linear time and single traversal of the array.
    * For Example:
    * Input: arr=[1,3,5,4,8,2,4,3,6,5], x=3, y=2 => Output: Minimum difference between index is 2
    * Input: arr=[1,3,5,4,8,2,4,3,6,5], x=2, y=5 => Output: Minimum difference between index is 3
    *
    * @param arr
    */
  def MinimumDifferenceBetweenIndexOfTwoGivenElementsPresentInTheArray(arr: Array[Int], x: Int, y: Int): Int = {
    /** the idea is to traverse the array and keep track of last occurrence of x and y.
        1. if element x is encountered, we find the absolute difference between current index of x and index of last occurrence of y and
           update the result if required.
        2. if element y is encountered, we find the absolute difference between current index of y and index of last occurrence of x and
           update the result if required. **/
    def go(index: Int, xLatestIndex: Int, yLatestIndex: Int, minDiff: Int): Int = {
      if(index == arr.length) minDiff
      else {
        arr(index) match {
          case num  if num == x => go(index + 1, index, yLatestIndex, minDiff = if (yLatestIndex != -1) Math.min(minDiff, Math.abs(index - yLatestIndex)) else minDiff)
          case num if num == y => go(index + 1, xLatestIndex, index, minDiff = if (xLatestIndex != -1) Math.min(minDiff, Math.abs(index - xLatestIndex)) else minDiff)
          case _ => go(index + 1, xLatestIndex, yLatestIndex, minDiff)
        }
      }
    }
    go(0, -1, -1, Integer.MAX_VALUE)
  }

  /**
    * Largest sum contiguous subarray
    * For Example:
    * Arr=[-2,-3,4,-1,-2,1,5,-3] => Largest subarray sum is [4,-1,-2,1,5]
    *
    * @param arr
    */
  def largestSumContiguousSubarray(arr: Array[Int]): Int = {
    /** the idea to check all positive contiguous segments of the array (by updating maxSofarTemp) and keep track of the maximum segment **/
    def go(index: Int, maxSoFar: Int, maxSoFarTemp: Int): Int = {
      if(index == arr.length) {
        maxSoFar
      } else {
        maxSoFarTemp + arr(index) match {
          case num if num >= maxSoFar  => go(index + 1, num, num)
          case num if num < maxSoFar => go(index + 1, maxSoFar, if(num > 0) num else 0)
        }
      }
    }
    go(0,0,0)
  }

  /**
    * Given an array of integers, partition the array into two sub-arrays having the same sum of elements.
    * For Example:
    * input=[6,-4,-3,2,3] => [6,-4] & [-3,2,3] having sum of 2
    *
    * @param arr
    */
  def partitionAnArrayIntoTwoSubArraysWithTheSameSum(arr: Array[Int]): Unit = {
    def go(index: Int): Unit = {
      if(index == arr.length) {
        println("None")
      } else {
        val (arr1,arr2) = arr.splitAt(index)
        if(arr1.sum == arr2.sum) {
          println(s"arr1=${arr1.mkString(",")}, arr2=${arr2.mkString(",")}")
        } else {
          go(index + 1)
        }
      }
    }
    go(0)
  }
 
   /**
    * Given an array an integer k, find the count of distinct elements in every sub-array of size k in the array.
    * For Example:
    * input:[2,1,2,3,2,1,4,5], k=5
    * output: {2,1,2,3,2} => 3, {1,2,3,2,1} => 3, {2,3,2,1,4} => 4, {3,2,1,4,5} => 5
    *
    * @param arr
    * @param k
    */
  def countOfDistinctElementsInEverySubArrayOfSizeK(arr: Array[Int], k: Int): Unit = {
    def naive() = Range(0, arr.length - k + 1).foreach(i => println(arr.slice(i, i + k).distinct.length))
    def efficient = {
      val map = collection.mutable.Map[Int,Int]()
      for(i <- 0 until k) {
        map += arr(i) -> (map.getOrElse(arr(i), 0) + 1)
      }
      println(map.size)
      for(i <- k until arr.length) {
        map.get(arr(i - k)).map(count => if(count == 1) map.remove(arr(i - k)) else map.updated(arr(i - k), count - 1))
        map += arr(i) -> (map.getOrElse(arr(i), 0) + 1)
        println(map.size)
      }
    }
    efficient
  }

  /**
    * Given an array of integers between 0 to 9, find two numbers with maximum sum formed by using all digits of the array.
    * The difference in number of digits of the two numbers should be +-1
    * For Example:
    * input: [4,6,2,7,9,8]
    * output: the two numbers with maximum sum are 974 and 862
    * input: [9,2,5,6,0,4]
    * output: the two numbers with maximum sum are 952 and 640
    *
    * @param arr
    */
  def twoNumbersWithMaximumSumFormedByArrayDigits(arr: Array[Int]): Unit = {
    val (one,two) = arr.sorted.reverse.zipWithIndex.partition(_._2 % 2 == 0)
    println(s"one=${one.map(_._1).mkString("")}, two=${two.map(_._1).mkString("")}")
  }

  /**
    * Shuffle an array according to the given order of elements
    * For Example:
    * input: [1,2,3,4,5]
    * pos: [3,2,4,1,0]
    * output: [5,4,2,1,3]
    *
    * @param arr
    */
  def shuffleAnArrayAccordingToTheGivenOrderOfElements(arr: Array[Int], pos: Array[Int]): Unit = {
    val result = Array.fill(arr.length)(0)
    for (i <- 0 until arr.length) {
      result(pos(i)) = arr(i)
    }
  }

  /**
    * k=2, I=[1,2,3,4,5,6]
    * output: I=[5,6,1,2,3,4]
    *
    * @param arr
    */
  def shiftArrayByK(k: Int, arr: Array[Int]): Array[Int] = {
    val from = k % arr.length
    val a = arr.takeRight(from)
    val b = arr.take(arr.length - from)
    a ++ b
  }

  def shiftArrayByKInPlace(k: Int, arr: Array[Int]): Array[Int] = {
    if(k == 0) arr
    else shiftArrayByKInPlace(k - 1, shiftArrayByOneInPlace(arr))
  }

  def shiftArrayByOneInPlace(arr: Array[Int]) = {
    for(i <- 0 until arr.length) {
      val temp = arr(i)
      arr(i) = arr(arr.length - 1)
      arr(arr.length - 1) = temp
    }

    arr
  }

  /**
    * Find the length of the non repeated numbers in an array
    * Input = [1,2,2,3,4,5,2,3]
    * Output = 4 [2,3,4,5] or [3,4,5,2] or [4,5,2,3]
    *
    * @param arr
    */
  def lengthOfNonRepeatedNumbersInAnArray(arr: Array[Int]): Int = {
    val set: collection.mutable.Set[Int] = mutable.Set.empty[Int]
    var max = 0
    var maxTemp = 0
    for(i <- 0 until arr.length) {
      if(!set.contains(arr(i))) {
        set += arr(i)
        maxTemp += 1
        if(maxTemp > max) {
          max = maxTemp
        }
      } else {
        set.clear()
        set += arr(i)
        maxTemp = 1
      }
    }
    max
  }

  /**
    * Given an array of sorted integers and find the closest value to the given number. Array may contain duplicate values and negative numbers.
    * Example : Array : 2,5,6,7,8,8,9
    * Target number : 5
    * Output : 5
    * Target number : 11
    * Output : 9
    * Target Number : 4
    * Output : 5
    *
    * @param arr
    * @param target
    */
  def closestValueToAGivenNumber(arr: Array[Int], target: Int): Int = {
    arr.foldLeft(arr(0)) { (a, b) =>
      if (Math.abs(target - b) < Math.abs(target -  a)) b else a
    }
  }

  /**
    * Given an array A of size N, and an integer X, you need to return the count of all subarrays which sums up to X
    * arr = [1,4,5,2,3,5,2], X = 5 => [1,4],[5],[2,3],[5]
    *
    * @param arr
    * @param X
    */
  def countSubArraysWhichSumsUpToX(arr: Array[Int], X: Int): Int = {
    def go(index: Int, sum: Int, count: Int): Int = {
      if(index >= arr.length) count
      else if(arr(index) == X || arr(index) + sum == X) go(index + 1, 0, count + 1)
      else if(arr(index) + sum < X) go(index + 1, arr(index) + sum, count)
      else go(index + 1, 0, count)
    }
    go(0, 0, 0)
  }
 
  /**
    * Given an array of integers, find the first missing positive integer in linear time and constant space.
    * In other words, find the lowest positive integer that does not exist in the array. The array can contain duplicates and negative numbers as well.
    * Example : Array : 3,4,-1,1
    * Output : 2
    * Example : Array : 200,-1,67,7,111,-3,4,-5
    * Output : 1
    * Example : Array : 1,2,0
    * Output : 3
    * Example : Array : 1,2,3
    * Output : 4
    * Example : Array : -1,-2,-3,-4,-5
    * Output : 1
    *
    * @param arr
    */
  def firstMissingPositiveIntegerInLinearTimeAndConstantSpace(arr: Array[Int]): Int = {

    // O(n) space !
    def withSet(): Int = {
      val set = scala.collection.mutable.Set(arr: _*) // put all in set
      (1 to arr.length).find(num => !set.contains(num)).getOrElse(arr.length + 1) // scan all numbers from 1 to n and check if exists in the set, if all exists, yield n + 1 as the next missing lowest number
    }

    // O(nlong) time !
    def sorting(): Int = {
      def go(arr: Array[Int], index: Int, next: Int): Int = {
        if(index == arr.length) next
        else if(arr(index) > next) next
        else if(arr(index) == next) go(arr, index + 1, next + 1)
        else go(arr, index + 1, next)
      }

      go(arr.sorted, 0, 1)
    }

    def inPlace(): Int = {
      // We use array elements as index.
      // To mark presence of an element x, we change the value at the index x to negative.
      // But this approach doesn’t work if there are non-positive (-ve and 0) numbers.
      // So we segregate positive from negative numbers as first step and then apply the approach.

      // Segregate positive numbers from others i.e., move all non-positive numbers to left side
      // 1 -1 -5 -3 3 4 2 8 => 1 8 2 4 3 | -3 -5 -1, lastPositiveIndex = 5
      val seregated = seregatePositves(arr)
      val lastPositiveIndex = seregated.zipWithIndex.find { case (num, index) => num <= 0}.map(_._2).getOrElse(0)
      for(i <- 0 until lastPositiveIndex) {
        if(arr(i) <= lastPositiveIndex && arr(i) - 1 >= 0 && arr(i) - 1 < lastPositiveIndex) {
          arr(arr(i) - 1) = arr(arr(i) - 1) * -1
        }
      }

      // Finally, we traverse the array once more from index 0 to end. In case we encounter a positive element at some index, we output index + 1
      arr.zipWithIndex.find { case (num, index) => num > 0}.map(_._2 + 1).getOrElse(1)
    }

    inPlace()
  }

  def seregatePositves(arr: Array[Int]): Array[Int] = {
    var left = 0
    var right = arr.length - 1

    while(left < right) {

      if(arr(right) <= 0) {
        right -= 1
      }

      if(arr(left) > 0) {
        left += 1
      }

      if(arr(right) > 0 && arr(left) <= 0) {
        val temp = arr(right)
        arr(right) = arr(left)
        arr(left) = temp
        right -= 1
        left += 1
      }
    }

    arr
  }
 
   /**
    * Return a new sorted merged list from K sorted lists, each with size N
    * Example : [[10, 15, 30], [12, 15, 20], [17, 20, 32]]
    * Result : [10, 12, 15, 15, 17, 20, 20, 30, 32]
    *
    * @param list
    */
  def newSortedMergedListFromKSortedListsEachWithSizeN(list: List[List[Int]]): List[Int] = {
    list.flatten.sorted
//    Another efficient solution is to use Min Heap. This Min Heap based solution has same time complexity which is O(nk Log k). But for different sized arrays, this solution works much better.
//
//      Following is detailed algorithm.
//    1. Create an output array of size n*k.
//    2. Create a min heap of size k and insert 1st element in all the arrays into the heap
//      3. Repeat following steps n*k times.
//    a) Get minimum element from heap (minimum is always at root) and store it in output array.
//      b) Replace heap root with next element from the array from which the element is extracted. If the array doesn’t have any more elements, then replace root with infinite. After replacing the root, heapify the tree.
  }

  /**
    * Given a list of integers, write a function that returns the largest sum of non-adjacent numbers. Numbers can be 0 or negative.
    * For example, [2, 4, 6, 2, 5] should return 13, since we pick 2, 6, and 5. [5, 1, 1, 5] should return 10, since we pick 5 and 5.
    *
    * @param arr
    */
  def largestSumOfNonAdjNumbers(arr: Array[Int]): Int = {

    def printAllPossibleCombinations(index: Int, prefix: String): Unit = {
      if(index >= arr.length) println(prefix)
      else {
        printAllPossibleCombinations(index + 2, prefix + arr(index))
        printAllPossibleCombinations(index + 1, prefix)
      }
    }

    def findLargest(index: Int, sum: Int): Int = {
      if(index >= arr.length) sum
      else {
        Math.max(findLargest(index + 2, sum + arr(index)), findLargest(index + 1, sum))
      }
    }

    findLargest(0, 0)
  }
 
   /**
    * There exists a staircase with N steps, and you can climb up either 1 or 2 steps at a time.
    * Given N, write a function that returns the number of unique ways you can climb the staircase.
    * The order of the steps matters.
    * For example, if N is 4, then there are 5 unique ways:
    * 1, 1, 1, 1
    * 2, 1, 1
    * 1, 2, 1
    * 1, 1, 2
    * 2, 2
    *
    */
  def climb(staircases: Seq[Int], N: Int): Unit = {
    def  go(staircases: Seq[Int], soFar: Seq[Int]): Unit = {
      if(soFar.nonEmpty && soFar.sum == N) println(soFar.mkString(","))
      else {
        val sum = Option(soFar).filter(_.nonEmpty).map(_.sum).getOrElse(0)
        for(i <- 0 until staircases.length) {
          if(sum + staircases(i) <= N) go(staircases, soFar :+ staircases(i))
        }
      }
    }

    go(staircases, Seq.empty)
  }

  /**
    * Given an array of integers and a number k, where 1 <= k <= length of the array, compute the maximum values of each subarray of length k.
    *
    * For example, given array = [10, 5, 2, 7, 8, 7] and k = 3, we should get: [10, 7, 8, 8], since:
    *
    * 10 = max(10, 5, 2)
    * 7 = max(5, 2, 7)
    * 8 = max(2, 7, 8)
    * 8 = max(7, 8, 7)
    * Do this in O(n) time and O(k) space. You can modify the input array in-place and you do not need to store the results. You can simply print them out as you compute them.
    * https://algorithms.tutorialhorizon.com/sliding-window-algorithm-track-the-maximum-of-each-subarray-of-size-k/
    * https://www.youtube.com/watch?v=5VDQxLAlfu0
    * Couldn't this technically run up to k times each loop, giving somewhere between O(n) and O(kn)? Is the worst-case time complexity actually O(kn) even for the deque solution?
    * Proof that this is O(N): Each iteration of the inner loops removes an item from Q. Since there are only  len(arr) added to Q in total, there can be at most len(arr) total iterations of the inner loops.
    */
  def maximumValuesOfEachSubArrayOfLengthK(arr: Array[Int], k: Int): Unit = {
    val dequeue: util.Deque[Int] = new util.LinkedList[Int]()

    for(i <- 0 until k) {

      while(!dequeue.isEmpty && arr(i) >= arr(dequeue.peekLast())) {
        dequeue.removeLast()
      }

      dequeue.addLast(i)
    }

    print(s"[${arr(dequeue.peekFirst())}")

    for(i <- k until arr.length) {

      while(!dequeue.isEmpty && (i - k) >= dequeue.peekFirst()) {
        dequeue.removeFirst()
      }

      while(!dequeue.isEmpty && arr(i) >= arr(dequeue.peekLast())) {
        dequeue.removeLast()
      }

      dequeue.addLast(i)

      print(s", ${arr(dequeue.peekFirst())}")
    }

    print(s"]")
  }
 
   /**
    * Given an array of time intervals (start, end) for classroom lectures (possibly overlapping), find the minimum number of rooms required.
    * For example:
    *
    * given [(30, 75), (0, 50), (60, 150)], you should return 2.
    *
    * given [(0, 50), (30, 75), (60, 150), (10, 87), (100, 120), (40, 80), (150, 200)], you should return 4.
    * Room_0: [(0, 50), (60, 150)]
    * Room_1: [(30, 75), (100, 120), (150, 200)]
    * Room_2: [(10, 87), (40, 80)]
    * Room_3: [(40, 80)]
    */
  object Classroom {

    case class Lecture(start: Int, end: Int)
    case class Room(lectures: scala.collection.mutable.ListBuffer[Lecture])

    def minimumNumberOfRoomsRequired(arr: Array[Lecture]): Int = {

      val rooms = scala.collection.mutable.ListBuffer[Room]()

      arr.foreach { lecture =>
        rooms.find(room => !roomCollide(lecture, room)) match {
          case Some(room) => room.lectures += lecture
          case None => rooms += roomFactory(lecture)
        }
      }

      def roomFactory(lecture: Lecture): Room = {
        Room(ListBuffer(lecture))
      }

      def roomCollide(lecture: Lecture, room: Room): Boolean = {
        room.lectures.exists(lectureCollide(lecture, _))
      }

      def lectureCollide(lecture1: Lecture, lecture2: Lecture): Boolean = {
        if(lecture1.end < lecture2.start || lecture1.start > lecture2.end) false else true
      }

      rooms.size
    }
  }
 
   /**
    * Given a dictionary of words and a string made up of those words (no spaces), return the original sentence in a list.
    * If there is more than one possible reconstruction, return any of them.
    * If there is no possible reconstruction, then return null.
    * For example, given the set of words 'quick', 'brown', 'the', 'fox', and the string "thequickbrownfox", you should return ['the', 'quick', 'brown', 'fox'].
    * Given the set of words 'bed', 'bath', 'bedbath', 'and', 'beyond', and the string "bedbathandbeyond", return either ['bed', 'bath', 'and', 'beyond] or ['bedbath', 'and', 'beyond'].
    */
  def originalSentenceInAList(bagOfWords: Set[String], str: String): Unit = {

    def bruteForce(index: Int): Unit = {
      if(index != str.length) {
        findWordFromIndex(index) match {
          case Some(word) => {
            println(word)
            bruteForce(index + word.length)
          }
          case _ => {
            println("reconstruction is not possible")
          }
        }
      }
    }

    def findWordFromIndex(index: Int): Option[String] = {
      def rec(index: Int, prefix: String): Option[String] = {
        if(bagOfWords.contains(prefix)) Some(prefix)
        else if(index == str.length) None
        else rec(index + 1, prefix + str.charAt(index))
      }
      rec(index, "")
    }

    bruteForce(0)
  }

  /**
    * You are given an M by N matrix consisting of booleans that represents a board. Each True boolean represents a wall. Each False boolean represents a tile you can walk on.
    * Given this matrix, a start coordinate, and an end coordinate, return the minimum number of steps required to reach the end coordinate from the start.
    * If there is no possible path, then return null. You can move up, left, down, and right. You cannot move through walls. You cannot wrap around the edges of the board.
    * For example, given the following board:
    * [f, f, f, f],
    * [t, t, f, t],
    * [f, f, f, f],
    * [f, f, f, f]
    * and start = (3, 0) (bottom left) and end = (0, 0) (top left), the minimum number of steps required to reach the end is 7,
    * since we would need to go through (1, 2) because there is a wall everywhere else on the second row.
    */
  def findPathWithinMatrixOfBooleans(mat: Array[Array[Boolean]], m: Int, n: Int, position: (Int,Int), endPoint: (Int,Int)): Option[Int] = {

    val visited = scala.collection.mutable.Set[(Int,Int)]()
    var distance = 0
    val queue = new MyQueue[(Int,Int)]

    def run(): Option[Int] = {
      if(queue.isEmpty) None
      else run2() orElse run()
    }

    def run2(): Option[Int] = {
      if(!queue.isEmpty) {
        val element = queue.remove
        if(element == endPoint) Some(distance)
        else if(visited.contains(element)) run2()
        else {
          distance += 1
          visited += element
          val row = element._1
          val col = element._2

          // left
          if(col > 0 && !mat(row)(col-1) && !visited.contains(row -> (col-1))) queue.add(row -> (col-1))
          // right
          if(col < n-1 && !mat(row)(col+1) && !visited.contains(row -> (col+1))) queue.add(row -> (col+1))
          // up
          if(row > 0 && !mat(row-1)(col) && !visited.contains(row-1 -> col)) queue.add(row-1 -> col)
          // down
          if(row < m-1 && !mat(row+1)(col) && !visited.contains(row+1 -> col)) queue.add(row+1 -> col)

          run2()
        }
      } else None
    }

    queue.add(position)
    run()
  }

  object JustifyTextApp {
    /**
      * Write an algorithm to justify text. Given a sequence of words and an integer line length k, return a list of strings which represents each line, fully justified.
      * More specifically, you should have as many words as possible in each line. There should be at least one space between each word.
      * Pad extra spaces when necessary so that each line has exactly length k.
      * Spaces should be distributed as equally as possible, with the extra spaces, if any, distributed starting from the left.
      * If you can only fit one word on a line, then you should pad the right-hand side with spaces.
      * Each word is guaranteed not to be longer than k.
      *
      * For example, given the list of words ["the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"] and k = 16, you should return the following:
      * ["the  quick brown", # 1 extra space on the left
      * "fox  jumps  over", # 2 extra spaces distributed evenly
      * "the   lazy   dog"] # 4 extra spaces distributed evenly
      */
    def justifyText(listOfWords: List[String], k: Int): List[String] = {
      listOfWordsToListOfLines(listOfWords, k).flatMap(padLine(_, k))
    }

    def listOfWordsToListOfLines(listOfWords: List[String], k: Int): List[List[String]] = {
      val linesBuffer = ListBuffer.empty[List[String]]
      var lineBuffer = ListBuffer.empty[String]
      var lineLength: Int = 0
      listOfWords.foreach { word =>
        val wordLength = word.length
        if(lineLength + wordLength > k) {
          linesBuffer += lineBuffer.toList
          lineBuffer.clear()
          lineLength = 0
        }
        lineBuffer += word
        lineLength += (if(lineLength > 0) 1 + wordLength else wordLength)
      }
      if(lineBuffer.nonEmpty) {
        linesBuffer += lineBuffer.toList
      }

      linesBuffer.toList
    }

    def padLine(line: List[String], k: Int): List[String] = {
      val result = padLineOnce(line, k)
      if(result.map(_.length).sum >= k) result
      else padLine(result, k)
    }

    def padLineOnce(line: List[String], k: Int): List[String] = {
      def pad(word: List[String], spacesLeft: Int, result: List[String]): List[String] = {
        if(spacesLeft <= 0) result ::: word
        else {
          word match {
            case Nil => result
            case head :: Nil => result ::: List(head)
            case head :: tail => pad(tail, spacesLeft - 1, result ::: List(head) ::: List(" "))
          }
        }
      }
      pad(line, numberOfSpacesToPad(line, k), Nil)
    }

    def numberOfSpacesToPad(line: List[String], k: Int): Int = {
      k - line.map(_.length).sum
    }
  }

  /**
    * Run-length encoding is a fast and simple method of encoding strings.
    * The basic idea is to represent repeated successive characters as a single count and character.
    * For example, the string "AAAABBBCCDAA" would be encoded as "4A3B2C1D2A".
    * Implement run-length encoding and decoding.
    * You can assume the string to be encoded have no digits and consists solely of alphabetic characters. You can assume the string to be decoded is valid.
    */
  def runLengthEncoding(str: String): String = {
    // it's not fully solved but the idea is clear
    val builder = new mutable.StringBuilder()

    def findLastIndexOfChar(char: Char, index: Int): Int = {
      if(index >= str.length) index
      else if(str.charAt(index) != char) index - 1
      else findLastIndexOfChar(char, index + 1)
    }

    def go(index: Int): String = {
      if(index >= str.length) builder.result()
      else {
        val char = str.charAt(index)
        val endIndex = findLastIndexOfChar(char, index)
        builder.append(char).append((endIndex - index) + 1)
        go(index + (endIndex - index) + 1)
      }
    }
    go(0)
  }

  /**
    * You are given an array of non-negative integers that represents a two-dimensional elevation map where each element is unit-width wall and the integer is the height.
    * Suppose it will rain and all spots between two walls get filled up.
    * Compute how many units of water remain trapped on the map in O(N) time and O(1) space.
    * For example, given the input [2, 1, 2], we can hold 1 unit of water in the middle.
    * Given the input [3, 0, 1, 3, 0, 5], we can hold 3 units in the first index, 2 in the second, and 3 in the fourth index (we cannot hold 5 since it would run off to the left), so we can trap 8 units of water.
    */
  def trappingRainWater(arr: Array[Int]): Int = {
    // https://www.geeksforgeeks.org/trapping-rain-water/
    // An element of array can store water if there are higher bars on left and right.
    // We can find amount of water to be stored in every element by finding the heights of bars on left and right sides.
    // The idea is to compute amount of water that can be stored in every element of array.
    // For example, consider the array {3, 0, 0, 2, 0, 4}, we can store three units of water at indexes 1 and 2, and one unit of water at index 3, and three units of water at index 4.
    // simple solution - traverse every array element and find the highest bars on left and right sides. Take the smaller of two heights.
    // The difference between smaller height and height of current element is the amount of water that can be stored in this array element. Time complexity of this solution is O(n2)

    def simpleSolution: Int = {

      var result = 0

      def findMax(startIndex: Int, endIndex: Int, max: Int = Integer.MIN_VALUE): Int = {
        if(startIndex >= endIndex) max
        else findMax(startIndex + 1, endIndex, if(arr(startIndex) > max) arr(startIndex) else max)
      }

      for(i <- 1 until arr.length - 1) {
        val leftMax = findMax(0, i)
        val rightMax = findMax(i + 1, arr.length)
        val smallest = Math.min(leftMax, rightMax)
        result += (smallest - arr(i))
      }

      result
    }

    simpleSolution
  }

  /**
    * The edit distance between two strings refers to the minimum number of character insertions, deletions, and substitutions required to change one string to the other.
    * For example, the edit distance between “kitten” and “sitting” is three: substitute the “k” for “s”, substitute the “e” for “i”, and append a “g”.
    * Given two strings, compute the edit distance between them.
    */
  def distanceBetweenTwoStrings(str1: String, str2: String): Int = {

    def considerAllOptions(indexStr1: Int, indexStr2: Int, countEdit: Int): Int = {
      // if first string is empty, the only option is to
      // insert all characters of second string into first
      if(indexStr1 == 0) indexStr2 + countEdit
      // if second string is empty, the only option is to
      // remove all characters of first string
      else if(indexStr2 == 0) indexStr1 + countEdit
      // if last characters of two strings are same, nothing
      // much to do. Ignore last characters and get count for
      // remaining strings.
      else if(str1.charAt(indexStr1) == str2.charAt(indexStr2)) considerAllOptions(indexStr1 - 1, indexStr2 - 1, countEdit)

      else Math.min (
        Math.min (
          // remove
          considerAllOptions(indexStr1 -1, indexStr2, countEdit + 1),
          // insert
          considerAllOptions(indexStr1, indexStr2 - 1, countEdit + 1)
        ),
        // replace
        considerAllOptions(indexStr1 -1, indexStr2, countEdit + 1)
      )
    }

    considerAllOptions(str1.length - 1, str2.length - 1, 0)
  }

  /**
    * Compute the running median of a sequence of numbers. That is, given a stream of numbers, print out the median of the list so far on each new element.
    * Recall that the median of an even-numbered list is the average of the two middle numbers.
    * For example, given the sequence [2, 1, 5, 7, 2, 0, 5], your algorithm should print out:
    * 2
    * 1.5
    * 2
    * 3.5
    * 2
    * 2
    * 2
    */
  def medianOfSequenceNumbers(arr: Array[Int]): Unit = {

    def calculateMedian(seqToCalculate: Array[Int]): Double = {
      val sorted = seqToCalculate.sorted
      // even
      if(sorted.length % 2 == 0) {
        val index1 = sorted.length / 2
        val index2 = index1 - 1
        (sorted(index1) + sorted(index2)) / 2.0
      } else {
        sorted(sorted.length / 2)
      }
    }

    for(i <- 1 to arr.length) {
      println(calculateMedian(arr.take(i)))
    }
  }

  /**
    * Given a string, find the palindrome that can be made by inserting the fewest number of characters as possible anywhere in the word.
    * If there is more than one palindrome of minimum length that can be made, return the lexicographically earliest one (the first one alphabetically).
    * For example, given the string "race", you should return "ecarace", since we can add three letters to it (which is the smallest amount to make a palindrome).
    * There are seven other palindromes that can be made from "race" by adding three letters, but "ecarace" comes first alphabetically.
    * As another example, given the string "google", you should return "elgoogle".
    */
  def palindromeFewestNumberOfCharacters(str: String): Unit = {
    // this is not the solution, but only to find the simple palindrome that can be found from the start / end

    def isPalindrome(str: String): Boolean = {
      str == str.reverse
    }

    def fromTheEnd(strBuilder: String): Unit = {
      if(isPalindrome(strBuilder)) {
        println(strBuilder)
      } else {
        val letterToAdd = str.charAt(strBuilder.length - str.length)
        fromTheEnd(str + letterToAdd + strBuilder.drop(str.length))
      }
    }

    def fromTheStart(strBuilder: String): Unit = {
      if(isPalindrome(strBuilder)) {
        println(strBuilder)
      } else {
        val letterToAdd = str.charAt(str.length - (strBuilder.length - str.length) - 1)
        fromTheStart(strBuilder.take(strBuilder.length - str.length) + letterToAdd + str)
      }
    }

    fromTheEnd(str)
    fromTheStart(str)
  }
 
   /**
    * Given an array of strictly the characters 'R', 'G', and 'B', segregate the values of the array so that all the Rs come first, the Gs come second, and the Bs come last.
    * You can only swap elements of the array.
    * Do this in linear time and in-place.
    * For example, given the array ['G', 'B', 'R', 'R', 'B', 'R', 'G'], it should become ['R', 'R', 'R', 'G', 'G', 'B', 'B'].
    */
  def segregateValuesOfArray(arr: Array[Char]): Array[Char] = {

    def byCount: Array[Char] = {
      // all counts can be done on one pass
      var RC = arr.count(_ == 'R')
      var GC = arr.count(_ == 'G')
      var BC = arr.count(_ == 'B')
      for(i <- 0 until arr.length) {
        if(RC > 0) {
          arr(i) = 'R'
          RC -= 1
        }
        else if(GC > 0) {
          arr(i) = 'G'
          GC -= 1
        }
        else if(BC > 0) {
          arr(i) = 'B'
          BC -= 1
        }
      }
      arr
    }

    def sort: Array[Char] = {
      def weight(char: Char): Int = {
        char match {
          case 'R' => 3
          case 'G' => 2
          case 'B' => 1
        }
      }

      arr.sortWith { case (c1, c2) => weight(c1) > weight(c2) }
    }

    byCount
  }

  /**
    * Given an array and a number k where k is smaller than size of array, we need to find the k’th smallest element in the given array.
    * It is given that ll array elements are distinct.
    * Input: arr[] = {7, 10, 4, 3, 20, 15}
    * k = 3
    * Output: 7
    */
  def findKSmallestElementInArray(arr: Array[Int], k: Int, l: Int, r: Int): Int = {

    // First, we’ll want to choose a pivot (usually the last element)
    // Then, we’ll need to create a left reference to the lowest index (the first) element. And we’ll need to create a right reference to the highest index (the last) element — excluding the pivot.
    // Next, we’ll need to compare left and right in relation to the pivot, independently.
    // For example, if the left reference is less than the pivot, then we know that it is smaller than the pivot, and will be in the correct partition. So, we can increment the left reference, moving one element over (to the right). The same goes for the right reference: if the element at the reference is greater than the pivot, we know it’ll be in the correct partition, so we can increment it one element over (to the left).
    // However — if both the left reference is greater than the pivot and the right reference is smaller than the pivot, we know that we’ve stumbled upon two elements that are out of order.
    // In this situation, we can swap the two elements at the left reference and at the right reference so that they’re in the correct places — and will subsequently end up in the correct partitions!
    // Once we finish going through all the elements, our left reference will “pass” our right reference; in other words, the index of the left reference will be greater than the index of the right reference, meaning that we’ve finished sorting the two partitions. At this point, we can move the pivot into its correct place by swapping it with the item at the left reference
    def partition(lowIndex: Int, highIndex: Int): Int = {
      val pivot = arr(highIndex)

      var left = lowIndex
      var right = highIndex - 1

      while(left < right) {

        while(left <= right && arr(left) < pivot)
          left += 1

        while(left <= right && arr(right) > pivot)
          right += -1

        if(left < right) {
          swap(left,right)
          left += 1
          right += -1
        }
      }

      swap(left, highIndex)

      left
    }

    def swap(i: Int, j: Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    val pivot = partition(l, r)
    // If position is same as k
    if(pivot == k) arr(pivot)
    // If position is more, recur for left subarray
    else if(pivot < k) findKSmallestElementInArray(arr, k, l, pivot)
    // Else recur for right subarray
    else findKSmallestElementInArray(arr, k, pivot, k - pivot)
  }

  /**
    * Given an array of integers where every integer occurs three times except for one integer, which only occurs once, find and return the non-duplicated integer.
    * For example, given [6, 1, 3, 3, 3, 6, 6], return 1. Given [13, 19, 13, 13], return 19.
    * Do this in O(N) time and O(1) space.
    */
  def nonDuplicatedInteger(arr: Array[Int]): Int = {
//    The number of elements must be 3n + 1 for some n.
//    Pick an element randomly.
//    Switch it to the first location.
//
//    - Divide the rest of the elements into 2 groups in-place. Left group will have smaller or equal ones and right group will have elements greater than the chosen one. You can do this in O(n).
//      - if the left group size including the chosen element is a multiple of 3, the answer is in the right group. Else it is in the left group.
//      - rinse and repeat above steps on the remaining half.
//
//    Overall complexity is (m + m/2 + ...) which is O(m) where m is the total number of elements.
    1
  }
}
