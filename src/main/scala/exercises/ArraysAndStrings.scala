package exercises

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
}
