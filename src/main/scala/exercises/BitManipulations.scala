package exercises

/**
  * Created by niv on 14/12/2017.
  */
object BitManipulations extends App {

  val test1 = 0x0001 << 1 // 2
  val test2 = 0x0001 >> 1 // 0

  /**
    * You are given two 32-bit numbers, N and M, and two bit positions, i and j. Write a
    * method to set all bits between i and j in N equal to M (e.g., M becomes a substring of
    * N located at i and starting at j).
    * Input: N = 10000000000, M = 10101, i = 2, j = 6
    * Output: N = 10001010100
    * @param n
    * @param m
    * @param i
    * @param j
    * @return new number according to the requirement
    */
  def setBitsBetweenRanges(n: Int, m: Int, i: Int, j: Int): Int = {
    var result = n
    var temp   = m
    for(index <- i to j) {
      val bit = (0x0001 & temp) << index // preparing a mask with a single bit on/off at the relevant index position
      result = result | bit              // applying the bit with n
      temp = temp >> 1                   // advancing m to get the next bit
    }
    result
  }

  /**
    * Return the number of bits on for a given number
    * @param number
    * @return number of bits on
    */
  def getNumberOfBitsOn(number: Int): Int = {
    def getNumberOfBitsOn(number: Int, counter: Int): Int = {
      if(number == 0) counter
      else {
        val bitOn = if((number & 1) == 1) 1 else 0
        getNumberOfBitsOn(number >> 1, counter + bitOn)
      }
    }
    getNumberOfBitsOn(number, 0)
  }

  /**
    * Explain what the following code does: ((n & (n-1)) == 0)
    * Answer -
    * n == 1. n – 1 == 0. 1 & 0 == 0.
    * n == 2. n – 1 == 1. 10 & 01 == 0.
    * n == 3. n – 1 == 2. 11 & 10 != 0.
    * n == 4. n – 1 == 3. 100 & 011 == 0.
    * n == 5. n – 1 == 4. 101 & 100 != 0.
    * n == 6. n – 1 == 5. 110 & 101 != 0.
    * n == 7. n – 1 == 6. 111 & 110 != 0.
    * n == 8. n – 1 == 7. 1000 & 0111 == 0.
    * Looking at those n that satisfies ((n & (n-1)) == 0), we find n == 1, 2, 4, 8, i.e. power of 2.
    */

  /**
    * Write a function to determine the number of bits required to convert integer A to
    * integer B.
    * Input: 31 (11111), 14 (1110)
    * Output: 2
    * Solution - perform xor on the two numbers and count the number of active bits
    * @param a
    * @param b
    * @return number of bits required to convert integer a to b
    */
  def getNumberOfBitsRequiredToConvertAToB(a: Int, b: Int): Int = {
    getNumberOfBitsOn(a ^ b)
  }

  /**
    * Swap adjacent bits of a given number
    * For example - 0000 0000 0000 0000 0000 0000 0000 1110 => 0000 0000 0000 0000 0000 0000 0000 1101
    * @param num
    * @return number which it's adjacent bits were swapped
    */
  def swapAdjacentBitsOfANumber(num: Int): Int = {
    val maskForEvenBits    = 0xAAAAAAAA
    val maskForOddBits     = 0x55555555
    val evenBitsTurnedOn   = num & maskForEvenBits // 1010
    val oddBitsTurnedOn    = num & maskForOddBits  // 0100
    val shiftEvenBitsRight = evenBitsTurnedOn >> 1 // 0101
    val shiftEvenBitsLeft  = oddBitsTurnedOn  << 1 // 1000
    shiftEvenBitsRight | shiftEvenBitsLeft // OR between results
  }
}