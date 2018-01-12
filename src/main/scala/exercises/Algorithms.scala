package exercises

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
}
