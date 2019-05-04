package exercises

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by niv on 12/01/2018.
  */
object Algorithms extends App {

  /**
    * Bubble sort
    * Bubble Sort is the simplest sorting algorithm that works by repeatedly swapping the adjacent elements if they are in wrong order.
    *
    * 5,2,6,1 (i0,j=0)
    * 2,5,6,1 (i=0,j=1)
    * 2,5,1,6 (i=0,j=2)
    * 2,5,1,6 (i=0,j=3)
    * 2,5,1,6 (i=1,j=0)
    * 2,1,5,6 (i=1,j=1)
    * 2,1,5,6 (i=1,j=2)
    * 2,1,5,6 (i=2,j=0)
    * 2,1,5,6 (i=2,j=1)
    * 2,1,5,6 (i=3,j=0)
    *
    * @param arr
    */
  def bubbleSort(arr: Array[Int]) = {
    for(i <- 0 until arr.length)
      for(j <- 0 until arr.length - 1 - i) { // Last i elements are already in place
        if(arr(j) > arr(j+1)) {
          // swap
          val temp = arr(j)
          arr(j)   = arr(j+1)
          arr(j+1)   = temp
        }
      }
  }

  /**
    * Insertion sort
    * It's like sorting cards
    *
    * 5,2,6,1 (i=1,j=1)
    * 2,5,6,1 (i=2,j=2)
    * 2,5,6,1 (i=2,j=1)
    * 2,5,6,1 (i=3,j=3)
    * 2,5,1,6 (i=3,j=2)
    * 2,1,5,6 (i=3,j=1)
    * 1,2,5,6 (i=3,j=0)
    *
    * @param arr
    */
  def insertionSort(arr: Array[Int]) = {
    for(i <- 1 until arr.length)
      for(j <- i until 0 by -1) {
        if(arr(j - 1) > arr(j)) {
          // swap
          val temp = arr(j)
          arr(j)   = arr(j -1)
          arr(j - 1)   = temp
        }
      }
  }

  /**
    * Quick sort
    * Choose any element of the array to be the pivot (like the last one).
    * Divide all other elements (except the pivot) into two partitions.
    * All elements less than the pivot must be in the first partition.
    * All elements greater than the pivot must be in the second partition.
    * Use recursion to sort both partitions.
    * Join the first sorted partition, the pivot, and the second sorted partition.
    *

    * @param arr
    */
  def quickSort(arr: Array[Int]) = {
//    def partition(low: Int, high: Int): Unit = {
//      var i = low
//      var j = high
//      val pivotVal = arr(low + (high - low) / 2) // choose pivot as
//      while(i <= j) {
//        while(arr(i) < pivotVal) i+=1
//        while(arr(j) > pivotVal) j+=(-1)
//        if(i <= j) {
//          val temp = arr(i)
//          arr(i) = arr(j)
//          arr(j) = temp
//          i+=1
//          j+=(-1)
//        }
//      }
//
//      if (low < j)
//        partition(low, j)
//      if (i < high)
//        partition(i, high)
//    }

    def swap(i: Int, j: Int) = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    def partition(low: Int, high: Int): Int = {
      val pivotValue = arr(high) // choose the last element to be the pivot
      var indexOfTheLastSmallerElementThenPivotInTheSubArray = low

      // iterate via the rest of the elements not including the last one as it is the pivot
      for(i <- low until high -1) {
        if(arr(i) < pivotValue) {
          swap(i, indexOfTheLastSmallerElementThenPivotInTheSubArray)
          indexOfTheLastSmallerElementThenPivotInTheSubArray += 1
        }
      }

      // put the pivot value at its correct position
      swap(indexOfTheLastSmallerElementThenPivotInTheSubArray, high)

      indexOfTheLastSmallerElementThenPivotInTheSubArray
    }

    def runQuickSort(low: Int, high: Int): Unit = {
      if(low < high) {
        val index = partition(low, high)
        runQuickSort(low, index - 1)  // recursion on the left side, not including the pivot
        runQuickSort(index + 1, high) // recursion on the right side, not including the pivot
      }
    }

    runQuickSort(0, arr.size - 1)
  }

  /**
    * Design an advanced data structure which has all the functionalities of a set and
    * additionally it is also able to return a random number from the set in constant time.
    * the data structure should have the following operations in O(1) time:
    * insert, remove, getRandom, contains
    * Solution -
    * **********
    * use both HashMap and ArrayList / Array / Double LinkedList.
    * insert :- Check if X is already present in HashMap --Time complexity O(1) . if not Present Then Add in end of ArrayList -- Time complexity O(1). add it in HashMap also x as key and last Index as a value -- Time complexity O(1).
    * remove :- Check if X is present in HashMap --Time complexity O(1). If present then find the its index and remove it from HashMap --Time complexity O(1). swap this element with last element in ArrayList and remove the last element --Time complexity O(1). Update the index of last Element in HashMap --Time complexity O(1).
    * getRandom :- Generate Random number from 0 to last index of ArrayList . return the ArrayList element at random index generated --Time complexity O(1).
    * contains :- See in HashMap for x as a key. --Time complexity O(1).
    */
  class SetWithConstantRandomElementFunctionality {
    private val array = new ArrayBuffer[Any]
    private val map   = mutable.Map.empty[Any,Int]

    def insert(value: Any) = {
      map.get(value) match {
        case None => {
          val index = array.size
          array += value
          map.put(value,index)
        }
        case _ =>
      }
    }

    def delete(value: Any) = {
      map.get(value) match {
        case Some(index) => {
          val lastValueIndex = array.size - 1
          if(index == lastValueIndex) {
            array.remove(index)
          } else {
            val lastValue = array(lastValueIndex)
            array(index) = lastValue
            array.remove(lastValueIndex)
            map.put(lastValue, index)
          }
          map.remove(value)
        }
        case _ =>
      }
    }

    def getRandom = if(array.isEmpty) throw new RuntimeException("empty array") else array(new Random().nextInt(array.size))

    def contains(value: Any) = map.contains(value)
  }

  // https://www.programcreek.com/2014/05/leetcode-implement-trie-prefix-tree-java/
  class Trie {
    case class TrieNode(char: Char, children: mutable.HashMap[Char,TrieNode] = new mutable.HashMap, var isLeaf: Boolean = false)
    val root = TrieNode(' ')

    def insert(word: String): Unit = {
      var children = root.children
      for(i <- 0 until word.length) {
        val charAt = word.charAt(i)
        val t = children.get(charAt) match {
          case Some(r) => r
          case None    => {
            val t = TrieNode(charAt)
            children += charAt -> t
            t
          }
        }
        children = t.children
        if(i == word.length - 1) t.isLeaf = true
      }
    }

    def insertRec(word: String): Unit = {

      def go(word: String, node: TrieNode): Unit = {
        if(word == "") node.isLeaf = true
        else {
          node.children.get(word.head) match {
            case None => { // create new TrieNode
              val newTreeNode = TrieNode(word.head)
              node.children += word.head -> newTreeNode
              go(word.substring(1), newTreeNode)
            }
            case Some(child) => go(word.substring(1), child)
          }
        }
      }

      go(word, root)
    }
  }

  /**
    * Given the mapping a = 1, b = 2, ... z = 26, and an encoded message, count the number of ways it can be decoded.
    * For example, the message '111' would give 3, since it could be decoded as 'aaa', 'ka', and 'ak'.
    * You can assume that the messages are decodable. For example, '001' is not allowed
    *
    * @param str
    */
  def mapping(str: String): Unit = {

    def go(str: String, prefix: String): Unit = {
      if(str == "") println(prefix)
      else if(str.length == 1) println(prefix + decode(str))
      else {
        if(str.take(1) == "1" || str.take(2).toInt <=  26) {
          go(str.substring(1), prefix + decode(str.take(1)))
          go(str.substring(2), prefix + decode(str.take(2)))
        }
        else go(str.substring(1), prefix + decode(str.take(1)))
      }
    }

    def decode(str: String): String = {
      (str.toInt - 1 + 'a').toChar.toString // "1" -> 0 + 'a' = 'a', 1 + 'a' = 'b', 1 + 'b' = 'c', ...
    }

    // this method prints all the possible decoding
    go(str, "")
  }
}
