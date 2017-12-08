import scala.reflect.ClassTag

/**
  * Created by nivnatan on 12/4/2017.
  */
object StackAndQueues extends App {

  /**
    * Implementation of an immutable stack
    */
  class MyStack[T] {
    case class StackNode[T](data: T, next: Option[StackNode[T]])
    private var top: Option[StackNode[T]] = None

    def push(data: T): Unit = {
      val newStackNode = StackNode(data, top)
      top = Some(newStackNode)
    }

    def pop: T = {
      top match {
        case Some(topElem) => {
          top = topElem.next
          topElem.data
        }
        case None => throw new IllegalArgumentException("stack is empty")
      }
    }

    def peek = top.map(_.data).getOrElse(throw new IllegalArgumentException("stack is empty"))

    def isEmpty = top.isEmpty
  }

  /**
    * Implementation of an immutable queue
    */
  class MyQueue[T] {
    case class QueueNode[T](data: T, var next: Option[QueueNode[T]] = None)
    private var top: Option[QueueNode[T]] = None
    private var last: Option[QueueNode[T]] = None

    def add(data: T): Unit = {
      val newQueueNode = QueueNode(data)
      last.map(_.next = Some(newQueueNode))
      last = Some(newQueueNode)
      if (top.isEmpty) top = last
    }

    def remove: T = {
      top.map { case topElem =>
        top = topElem.next
        topElem.data
      }.getOrElse(throw new IllegalArgumentException("queue is empty"))
    }

    def peek = top.map(_.data).getOrElse(throw new IllegalArgumentException("stack is empty"))

    def isEmpty = top.isEmpty
  }

  /**
    * Three in One: Describe how you could use a single array to implement three stacks.
    * Array3Stacks - splits the array into 3 parts
    */
  class Array3Stacks[T](capacity: Int)(implicit m: ClassTag[T]) {
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

  /**
    * How would you design a stack which, in addition to push and pop, has a function min
    * which returns the minimum element? Push, pop and min should all operate in 0(1) time.
    * Solution - the idea is to hold two stacks, one to hold all the stack elements, and the other the minumum elements at any given point
    * For example: mainStack = 3->2->4->1->5, minStack = 3->2->2->1->1
    */
  class MinStack {
    private val minStack  = new MyStack[Int]
    private val mainStack = new MyStack[Int]

    def push(data: Int) = {
      if(minStack.isEmpty && mainStack.isEmpty) {
        mainStack.push(data)
        minStack.push(data)
      } else {
        val min = minStack.peek
        if(data < min) minStack.push(data) else minStack.push(min)
        mainStack.push(data)
      }
    }

    def min = minStack.peek

    def pop = {
      mainStack.pop
      minStack.pop
    }
  }

  /**
    * Queue via Stacks: Implement a MyQueue class which implements a queue using two stacks.
    * Solution - we can either make the add or remove costly. making the remove is cheaper
    * explanation if needed - http://www.geeksforgeeks.org/queue-using-stacks/
    */
  class MyQueueViaTwoStacks[T] {

    private val stackA = new MyStack[T]
    private val stackB = new MyStack[T]

    def add(data: T): Unit = stackA.push(data)

    def remove: T = {
      require(!stackA.isEmpty || !stackB.isEmpty)
      if(stackB.isEmpty) {
        while(!stackA.isEmpty) stackB.push(stackA.pop)
        stackB.pop
      } else {
        stackB.pop
      }
    }

    def peek = {
      if(!stackB.isEmpty) {
        stackB.peek
      } else {
        val data = this.remove
        stackB.push(data)
        data
      }
    }

    def isEmpty = stackA.isEmpty && stackB.isEmpty
  }

  /**
    * Sort Stack: Write a program to sort a stack such that the smallest items are on the top. You can use
    * an additional temporary stack, but you may not copy the elements into any other data structure
    * (such as an array). The stack supports the following operations: push, pop, peek, and isEmpty.
    */
  class SortStack {

    private val sortedStack = new MyStack[Int]
    private val tempStack   = new MyStack[Int]

    def push(data: Int): Unit = {
      if(sortedStack.isEmpty) {
        sortedStack.push(data)
      }
      else {
        if(sortedStack.peek > data) sortedStack.push(data)
        else {
          while(!sortedStack.isEmpty) tempStack.push(sortedStack.pop)   // Push everything to temp stack which becomes sorted by max
          while(tempStack.peek >= data) sortedStack.push(tempStack.pop) // Push all elements bigger then data back to sortedStack
          sortedStack.push(data)                                        // Push data to sortedStack
          while(!tempStack.isEmpty) sortedStack.push(tempStack.pop)     // Push the remaining elements from tempStack back to sortedStack
        }
      }
    }

    def pop: Int = sortedStack.pop

    def peek = sortedStack.peek

    def isEmpty = sortedStack.isEmpty
  }


  val stack = new SortStack
  stack.push(3)
  stack.push(2)
  stack.push(40)
  stack.push(1)
  stack.push(5)

  println(stack.pop)
  println(stack.pop)
  println(stack.pop)
  println(stack.pop)
  println(stack.pop)


//  val queue = new MyQueueViaTwoStacks[Int]
//  queue.add(3)
//  queue.add(2)
//  queue.add(4)
//  queue.add(1)
//  queue.add(5)
//
//  println(s"peak=${queue.peek}")
//  println(queue.remove)
//  println(s"peak=${queue.peek}")
//  println(queue.remove)
//  println(s"peak=${queue.peek}")
//  println(queue.remove)
//  println(s"peak=${queue.peek}")
//  println(queue.remove)
//  println(s"peak=${queue.peek}")
//  println(queue.remove)
}
