/**
  * Created by nivnatan on 12/4/2017.
  */
object StackAndQueues {

  class Array3Stacks[T](capacity: Int) {
    private val arr = new Array[T](capacity)
    private var firstIndex = 0
    private var secondIndex = 1
    private var thirdIndex = 2

    def push(data: T, stack: Int) = {
      stack match {
        case 1 => pushToStack(data, firstIndex); firstIndex += 3
        case 2 => pushToStack(data, secondIndex); secondIndex += 3
        case 3 => pushToStack(data, thirdIndex); thirdIndex += 3
        case _ => throw new RuntimeException("stack index is out of range")
      }
    }

    def pop(stack: Int) = {
      stack match {
        case 1 => popFromStack(firstIndex); firstIndex -= 3
        case 2 => popFromStack(secondIndex); secondIndex -= 3
        case 3 => popFromStack(thirdIndex); thirdIndex -= 3
        case _ => throw new RuntimeException("stack index is out of range")
      }
    }

    private def pushToStack(data: T, index: Int) = {
      require(index <= capacity - 1)
      arr(index) = data
    }

    private def popFromStack(index: Int) = {
      require(index >= 0)
      arr(index)
    }
  }
}
