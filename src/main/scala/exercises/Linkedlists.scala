package exercises

import scala.annotation.tailrec

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
      runner(head.data, head)
      head.next.map(iterate(_))
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
    while(mainPointer.flatMap(_.next).isDefined) {
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

  /**
    * Intersection: Given two (singly) linked lists, determine if the two lists intersect. Return the intersecting
    * node. Note that the intersection is defined based on reference, not value. That is, if the kth
    * node of the first linked list is the exact same node (by reference) as the jth node of the second
    * linked list, then they are intersecting.
    * http://www.knowsh.com/Notes/160273/Determine-If-The-Two-Linked-Lists-Intersect
    * https://github.com/zhou-dong/algorithm/blob/master/algorithm/src/org/dzhou/research/cci/linkedlist/Intersection.java
    *
    * @param list1
    * @param list2
    * @return node reference of where the intersection begins, or none in case no intersection occurs
    */
  def intersectionByRef(list1: Node, list2: Node): Option[Node] = {

    // return the size and tail node for a given list
    def getSizeAndTail(head: Node, counter: Int = 1): (Node,Int) = {
      head.next match {
        case Some(next) => getSizeAndTail(next, counter + 1)
        case None       => head -> counter
      }
    }

    // advancing the list by k and return the k node (relays that it exists for sure)
    def getKthNodeOfLongerList(head: Node, k: Int): Node = {
      if(k == 0) head else getKthNodeOfLongerList(head.next.get, k - 1)
    }

    val list1SizeAndTail    = getSizeAndTail(list1)
    val list2SizeAndTail    = getSizeAndTail(list2)

    if(list1SizeAndTail._1 != list2SizeAndTail._1) {
      None
    } else {
      val longerList          = if(list1SizeAndTail._2 > list2SizeAndTail._2) list1 else list2
      val shorterList         = if(list1SizeAndTail._2 > list2SizeAndTail._2) list2 else list1
      val longerListAdvanced  = getKthNodeOfLongerList(longerList, Math.abs(list1SizeAndTail._2 - list2SizeAndTail._2)) // at this point the pointer of longer list points on an equal number of elements list as the shorter one.

      def findListsIntersections(list1: Option[Node], list2: Option[Node]): Option[Node] = {
        (list1,list2) match {
          case (None,None) => None
          case (list1,list2) if(list1 == list2) => list1
          case (list1,list2)  => findListsIntersections(list1.flatMap(_.next), list2.flatMap(_.next))
        }
      }

      findListsIntersections(Some(longerListAdvanced), Some(shorterList))
    }
  }

  /**
    * Loop Detection: Given a circular linked list, implement an algorithm that returns the node at the
    * beginning of the loop.
    * DEFINITION
    * Circular linked list: A (corrupt) linked list in which a node's next pointer points to an earlier node, so
    * as to make a loop in the linked list.
    * EXAMPLE
    * Input: // 1 -> 2 -> 3 -> 4 -> 5 -> 3
    * Output: 3
    *
    * @param head
    * @return the node that caused the loop, or None if no looping occurs
    */
  def loopDetection(head: Node): Option[Node] = {
    // we can solve this by maintaining set of already visited nodes,
    // or use floyd algorithm to detect a loop - i.e. - Traverse linked list using two pointers.
    // move one pointer by one and other pointer by two.  If these pointers meet at some node then there is a loop.
    // if pointers do not meet then linked list does not have loop - let's implement this
    // when the two pointers met, they met at some point within the circle, now we need to find the beginning of the cycle.
    // we take back the slow pointer to the beginning, and advance fast and slow pointer by one. when they meet - that is the beginning of the cycle.
    // explanation - https://stackoverflow.com/questions/2936213/explain-how-finding-cycle-start-node-in-cycle-linked-list-work
    def findFirstEncounterNodeOfSlowAndFastPointers(slow: Option[Node], fast: Option[Node], moveByOne: Boolean): Option[Node] = {
      (slow, fast) match {
        case (Some(s),Some(f)) if(s==f) => Some(s)
        case (Some(s),Some(f))          => findFirstEncounterNodeOfSlowAndFastPointers(s.next, if(moveByOne) f.next else f.next.flatMap(_.next), moveByOne)
        case _                          => None
      }
    }

    val firstEncounter = findFirstEncounterNodeOfSlowAndFastPointers(head.next, head.next.flatMap(_.next), false)
    findFirstEncounterNodeOfSlowAndFastPointers(Some(head), firstEncounter, true)
  }

  /**
    * Reverse a linkedList (iteratively and recursively)
    *
    * @param head
    * @return head of reversed linkedList
    */
  def reverseListIterative(head: Node): Node = {
    @tailrec
    def reverseListIterativeTailRec(head: Node, prev: Option[Node]): Node = {
      val headNext = head.next
      head.next = prev
      headNext match {
        case Some(next) => reverseListIterativeTailRec(next, Some(head))
        case None       => head
      }
    }

    def reverseListIterativeRec(head: Node): Node = {
      head.next match {
        case None => head // Return C
        case Some(next) => {
          // find the last element to be the nea head
          val newHead = reverseListIterativeRec(next)
          // now in reverse order, starting from the second element from the end (which is B) do: {A -> B -> C -> None}
          next.next = Some(head) // B <- C
          head.next = None // B -> None
          newHead // Return C
        }
      }
    }

    reverseListIterativeRec(head)
  }

  /**
    * Given two sorted linked lists, how can you combine them into one big sorted list? do not create additional nodes
    * L1: 1 -> 20 -> 34 -> 42 -> 54
    * L2: 4 -> 16 -> 35
    * Result: 1 -> 4 -> 16 -> 20 -> 34 -> 35 -> 42 -> 54
    *
    * @param head1
    * @param head2
    * @return head of new sorted list
    */
  def sortTwoLinkedlistsUsingArray(head1: Node, head2: Node): Node = {
    val arrayResult = collection.mutable.ArrayBuffer.empty[Node]

    def fillRestOfList(head: Node): Unit = {
      arrayResult += head
      head.next.map(fillRestOfList)
    }

    def sortTwoLinkedlistsRec(head1: Node, head2: Node): Unit = {
      if(head1.data <= head2.data) {
        arrayResult += head1
        head1.next.map(sortTwoLinkedlistsRec(_, head2)).getOrElse { fillRestOfList(head2) }
      } else {
        arrayResult += head2
        head2.next.map(sortTwoLinkedlistsRec(head1, _)).getOrElse { fillRestOfList(head1) }
      }
    }

    sortTwoLinkedlistsRec(head1: Node, head2: Node)
    arrayResult.reduceLeft { (a: Node,b: Node) => a.next = Some(b); b }
    arrayResult.head
  }

  /**
    * Given two sorted linked lists, how can you combine them into one big sorted list? do not create additional nodes
    * L1: 1 -> 20 -> 34 -> 42 -> 54
    * L2: 4 -> 16 -> 35
    * Result: 1 -> 4 -> 16 -> 20 -> 34 -> 35 -> 42 -> 54
    *
    * @param head1
    * @param head2
    * @return head of new sorted list
    */
  def sortTwoLinkedlists(head1: Node, head2: Node): Node = {

    def sortTwoLinkedlistsRec(headNew: Node, head1New: Option[Node], head2New: Option[Node]): Unit = {
      (head1New, head2New) match {
        case (None, None)              =>
        case (Some(head1), None)       => headNew.next = Some(head1); sortTwoLinkedlistsRec(head1, head1.next, None)
        case (None, Some(head2))       => headNew.next = Some(head2); sortTwoLinkedlistsRec(head2, None, head2.next)
        case (Some(head1),Some(head2)) => {
          if(head1.data < head2.data) {
            headNew.next = Some(head1)
            sortTwoLinkedlistsRec(head1, head1.next, Some(head2))
          } else {
            headNew.next = Some(head2)
            sortTwoLinkedlistsRec(head2, Some(head1), head2.next)
          }
        }
      }
    }

    val (headNew, head1New, head2New) = if(head1.data < head2.data) (head1, head1.next, Some(head2)) else (head2, Some(head1), head2.next) // initialize first head first
    sortTwoLinkedlistsRec(headNew, head1New, head2New)
    headNew
  }

  /**
    * Sort linkedlist containing 0's, 1's and 2's in single traversal
    * L1: 1 -> 0 -> 0 -> 2 -> 1
    * Result: 0 -> 0 -> 1 -> 1 -> 2
    *
    * @param head
    * @return head of new sorted list
    */
  def sortZerosOnesAndTwosLinkedlist(head: Node): Option[Node] = {
    // maintain 3 pointers and 3 heads. iterate through the list and for each node: change next to None and append to the correct list.
    // at the end of the iteration, append pointers and return head of new list
    var zeroNodes, oneNodes, twoNodes: Option[Node] = None
    var zeroHead, oneHead, twoHead   : Option[Node] = None

    def onZero(node: Node) = {
      zeroNodes match {
        case Some(nodes) => {
          nodes.next = Some(node)
          zeroNodes =  nodes.next
        }
        case None => {
          zeroHead = Some(node)
          zeroNodes = Some(node)
        }
      }
    }

    def onOne(node: Node) = {
      oneNodes match {
        case Some(nodes) => {
          nodes.next = Some(node)
          oneNodes =  nodes.next
        }
        case None => {
          oneHead = Some(node)
          oneNodes = Some(node)
        }
      }
    }

    def onTwo(node: Node) = {
      twoNodes match {
        case Some(nodes) => {
          nodes.next = Some(node)
          twoNodes =  nodes.next
        }
        case None => {
          twoHead = Some(node)
          twoNodes = Some(node)
        }
      }
    }

    def sortZerosOnesAndTwosLinkedlistRec(headO: Option[Node]): Option[Node] = {
      headO match {
        case Some(h) => {
          val next = h.next // remember the next
          h.next = None     // set next to None
          h.data match {
            case 0 => onZero(h)
            case 1 => onOne(h)
            case 2 => onTwo(h)
          }
          sortZerosOnesAndTwosLinkedlistRec(next)
        }
        case None => {
          oneNodes.map(_.next = twoHead)
          zeroNodes.map(_.next = oneHead)
          zeroHead orElse oneHead orElse twoHead
        }
      }
    }
    sortZerosOnesAndTwosLinkedlistRec(Some(head))
  }

  /**
    * Print all combinations of phrases than can be formed by picking words from each of the given lists
    * L1: "john" -> "jo"
    * L2: "runs" -> "walks"
    * Result: "john runs", "john walks", "jo runs", "jo walks",
    *
    * @param heads
    */
  def printAllCombinationsOfLists(heads: List[List[String]]): Unit = {
    def printAllCombinationsOfListsRec(prefix: String, heads:List[List[String]]): Unit = {
      heads match {
        case Nil                  =>
        case headList :: Nil      => headList.foreach(w => println(prefix + " " + w))
        case headList :: restList => headList.foreach(w => printAllCombinationsOfListsRec(prefix + " " + w, restList))
      }
    }
    printAllCombinationsOfListsRec("", heads)
  }

  /**
    * Reverse a linked list in groups of K
    * Example: If the list is 5->6->7->8->9->10->11->12->null and k is 3, then output would be:
    * 7->6->5->10->9->8->12->11->null
    *
    * @param head
    */
  def reverseLinkedListInGroupsOfK(head: Node): Node = {
    def reverseLinkedListInGroupsOfKRec(head: Node): Node = {
      (head.next, head.next.flatMap(_.next)) match {
        case (Some(next),Some(nextNext)) => {
          val restOfList = nextNext.next
          nextNext.next = Some(next)
          next.next = Some(head)
          head.next = restOfList.map(reverseLinkedListInGroupsOfKRec)
          nextNext
        }
        case _ => head
      }
    }
    reverseLinkedListInGroupsOfKRec(head)
  }

  /**
    * Reverse a linked list
    *
    * @param head
    */
  def reverseLL(head: Node) = {
    def reverseRec(previousElem: Option[Node], currentElem: Node): Node = {
      val nextElem = currentElem.next
      currentElem.next = previousElem
      nextElem.map(reverseRec(Some(currentElem), _)).getOrElse(currentElem)
    }
    reverseRec(None, head)
  }

  val node1 = Node(1)
  val node2 = Node(20)
  val node3 = Node(34)
  val node4 = Node(42)
  val node5 = Node(54)

  val node6 = Node(4)
  val node7 = Node(16)
  val node8 = Node(35)

  node1.next = Some(node2)    // 1 -> 20 -> 34 -> 42 -> 54
  node2.next = Some(node3)
  node3.next = Some(node4)
  node4.next = Some(node5)

  node6.next = Some(node7)
  node7.next = Some(node8)   // 4 -> 16 -> 35

  val asdas = "dsa"
  val res = reverseLL(node1)
  val asdafas = "das "
}
