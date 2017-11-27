import scala.annotation.tailrec
import scala.collection.immutable.List

/**
  * Created by niv on 23/11/2017.
  */
object Linkedlists extends App {

  case class Node(data: Int, var next: Option[Node] = None) {
    def appendToTail(data: Int) = {
      @tailrec
      def go(currentNode: Node): Unit =  {
        currentNode.next match {
          case None           => currentNode.next = Some(Node(data, None))
          case Some(nextNode) => go(nextNode)
        }
      }
      go(this)
    }
  }

  def deleteNode(head: Node, data: Int): Option[Node] = {
    @tailrec
    def go(currentNode: Node): Node = {
      currentNode.next match {
        case None => head
        case Some(nextNode) => {
          if (nextNode.data == data) {
            currentNode.next = nextNode.next
            head
          } else go(nextNode)
        }
      }
    }
    if(head.data == data) head.next else Some(go(head))
  }

  def printLinkedList(head: Node) = {
    @tailrec
    def go(currentNode: Node): Unit =  {
      print(currentNode.data + " ")
      currentNode.next match {
        case None           =>
        case Some(nextNode) => go(nextNode)
      }
    }
    go(head)
  }

  /**
    * Write code to remove duplicates from an unsorted linked list
    *
    * @param head
    * @return linked list head
    */
  def distinct(head: Node) = {
    val set = scala.collection.mutable.Set.empty[Int]
    def go(currentNode: Option[Node]): Node = {
      currentNode.flatMap(_.next) match {
        case None           => head
        case Some(nextNode) => {
          if(set.contains(nextNode.data)) {
            currentNode.map(_.next = nextNode.next)
          } else {
            currentNode.map(set += _.data)
          }
          go(currentNode.flatMap(_.next))
        }
      }
    }
    go(Some(head))
  }

  /**
    * Write code to remove duplicates from an unsorted linked list without using buffer. (2 pointers approach)
    *
    * @param head
    * @return linked list head
    */
  def distinct2Pointers(head: Node): Node = {
    def runner(data: Int, head: Node): Unit = {
      head.next.map { next =>
        if(next.data == data) head.next = next.next
        head.next.map(runner(data, _))
      }
    }

    def iterate(head: Node): Unit = {
      head.next.map { next =>
        runner(head.data, head)
        head.next.map(distinct2Pointers(_))
      }
    }

    iterate(head)
    head
  }

  /**
    * Implement an algorithm to find the kth to last element of a singly linked list.
    *
    * @param head
    * @return linked list head
    */
  def kthElementFromEnd(head: Node, k: Int) = {
    var mainPointer = Option(head)
    var slowPointer = Option(head)
    for(i <- 0 until k) mainPointer = mainPointer.flatMap(_.next)
    while(mainPointer.flatMap(_.next) isDefined) {
      mainPointer = mainPointer.flatMap(_.next)
      slowPointer = slowPointer.flatMap(_.next)
    }
    slowPointer.map(_.data)
  }
 
   /**
    * Delete Middle Node: Implement an algorithm to delete a node in the middle (i.e., any node but
    * the first and last node, not necessarily the exact middle) of a singly linked list, given only access to
    * that node.
    * EXAMPLE
    * Input: the node c from the linked list a - >b- >c - >d - >e- >f
    * Result: nothing is returned, but the new linked list looks like a - >b- >d - >e- >f
    *
    * @param head
    * @return linked list head
    */
  def deleteMiddleNode(head: Node) = {
    def go(head: Node, runner: Node, isMove: Int): Unit = {
      runner.next match {
        case Some(next) => head.next.map(h => go(if(isMove % 2 == 0) h else head, next, isMove + 1))
        case None       => head.next = head.next.flatMap(_.next)
      }
    }
    head.next.flatMap(_.next).map(go(head, _, 1)) // advance runner pointer by two before starting because we want to point to the first element before the middle element
  }

  /**
    * Partition: Write code to partition a linked list around a value x, such that all nodes less than x come
    * before all nodes greater than or equal to x. lf x is contained within the list, the values of x only need
    * to be after the elements less than x (see below). The partition element x can appear anywhere in the
    * "right partition"; it does not need to appear between the left and right partitions.
    * EXAMPLE
    * Input: 3 -> 5 -> 8 -> 5 -> 10 -> 2 -> 1 [partition = 5)
    * Output: 3 -> 1 -> 2 -> 10 -> 5 -> 5 -> 8
    *
    * @param head
    * @param x
    * @return linked list head
    */
  def partition(head: Node, x: Int) = {

    def appendToListOption(head: Option[Node], data: Int): Option[Node] = {
      def appendToList(head: Node): Unit = head.next.map(appendToList).getOrElse(head.next = Some(Node(data)))
      head.map(appendToList)
      head orElse Some(Node(data))
    }

    def concatListsOption(list1: Option[Node], list2: Option[Node]): Option[Node] = {
      def appendToList(head: Node): Unit = head.next.map(appendToList).getOrElse(head.next = list2)
      list1.map(appendToList)
      list1 orElse list2
    }

    def go(lessThenNodes: Option[Node], greaterThenNodes: Option[Node], currentNode: Option[Node]): Option[Node] = {
      currentNode.map { node =>
        if(node.data >= x) go(lessThenNodes, appendToListOption(greaterThenNodes, node.data), node.next)
        else go(appendToListOption(lessThenNodes, node.data), greaterThenNodes, node.next)
      }.getOrElse(concatListsOption(lessThenNodes, greaterThenNodes))
    }
    
    go(None, None, Some(head))
  }

  /**
    * You have two numbers represented by a linked list, where each node contains a single
    * digit. The digits are stored in reverse order, such that the 1’s digit is at the head of
    * the list. Write a function that adds the two numbers and returns the sum as a linked
    * list.
    * EXAMPLE
    * Input: (3 -> 1 -> 5), (5 -> 9 -> 2)
    * Output: 9 -> 0 -> 7
    *
    * @param list1
    * @param list2
    * @return linked list that represents the sum of the tow lists
    */
  def sumLists(list1: Node, list2: Node) = {
    def getListNumber(head: Node, number: Int = 0): Int = {
      head.next match {
        case Some(next) => getListNumber(next, (number * 10) + head.data)
        case None       => (number * 10) + head.data
      }
    }

    def createListFromNumber(num: Int) = {
      num
        .toString
        .map { case digit => Node(digit -'0') }
        .foldLeft((None: Option[Node], None: Option[Node])) { case ((head,runner), node) =>
          runner.map { r =>
            r.next = Some(node)
            (head, r.next)
          }.getOrElse((Some(node), Some(node)))
        }._1
    }

    val list1Number = getListNumber(list1)
    val list2Number = getListNumber(list2)
    createListFromNumber(list1Number + list2Number)
  }

  /**
    * You have two numbers represented by a linked list, where each node contains a single
    * digit. The digits are stored in reverse order, such that the 1’s digit is at the head of
    * the list. Write a function that adds the two numbers and returns the sum as a linked
    * list.
    * EXAMPLE
    * First List: 7->5->9->4->6  // represents number 64957
    * Second List: 8->4 //  represents number 48
    * Resultant list: 5->0->0->5->6  // represents number 65005
    *
    * @param list1
    * @param list2
    * @return linked list that represents the sum of the tow lists
    */
  def sumListsRec(list1: Node, list2: Node) = {
    def go(list1O: Option[Node], list2O: Option[Node], headResult: Option[Node], carry: Int): Option[Node] = {
      (list1O, list2O) match {
        case (None, None)   => if(carry > 0) Some(Node(1, headResult)) else headResult
        case (list1, list2) => {
          val sumWithCarry = list1.map(_.data).getOrElse(0) + list2.map(_.data).getOrElse(0) + carry
          val sumWithoutCarry = sumWithCarry % 10
          val isCarry = sumWithCarry > 9
          val newNode = Node(sumWithoutCarry, headResult)
          go(list1.flatMap(_.next), list2.flatMap(_.next), Some(newNode), if(isCarry) 1 else 0)
        }
      }
    }
    go(Some(list1), Some(list2), None, 0)
  }

  /**
    * Implement a function to check if a linked list is a palindrome
    *
    * @param head
    * @return true / false if the linked list is a palindrome
    */
  def isListPalindromeStr(head: Node): Boolean = {
    def getPalindromeStr(head: Node, result: StringBuilder): String = {
      head.next match {
        case Some(next) => getPalindromeStr(next, result.append(next.data.toString))
        case None => result.toString
      }
    }
    val listStr = getPalindromeStr(head, (new StringBuilder).append(head.data))
    listStr == listStr.reverse
  }

  /**
    * Implement a function to check if a linked list is a palindrome using reverse list
    *
    * @param head
    * @return true / false if the linked list is a palindrome
    */
  def isListPalindromeReverseList(head: Node): Boolean = {
    def reverseList(head: Node, result: Node): Node = {
      head.next match {
        case Some(next) => reverseList(next, Node(next.data, Some(result)))
        case None       => result
      }
    }

    def checkListsPalindrome(list1: Node, list1Reverse: Node): Boolean = {
      if(list1.data != list1Reverse.data) false
      else {
        (list1.next, list1Reverse.next) match {
          case (Some(l1Next), Some(l1ReverseNext)) => checkListsPalindrome(l1Next, l1ReverseNext)
          case (None, None)                        => true
          case _                                   => false
        }
      }
    }

    checkListsPalindrome(head, reverseList(head, Node(head.data)))
  }

  val node1 = Node(1)
  val node2 = Node(2)
  val node3 = Node(2)
  val node4 = Node(2)
  val node5 = Node(2)
  val node6 = Node(1)
  val node7 = Node(9)
  val node8 = Node(9)

  node1.next = Some(node2)
  node2.next = Some(node3)
  node3.next = Some(node4)
  node4.next = Some(node5)
  node5.next = Some(node6)
  //node6.next = Some(node7)
  //node7.next = Some(node8)


  val asdas = isListPalindromeReverseList(node1)


  val ttttt = ""


}
