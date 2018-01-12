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
}
