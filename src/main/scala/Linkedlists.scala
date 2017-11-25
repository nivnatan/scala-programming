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

  val node1 = Node(3)
  val node2 = Node(5)
  val node3 = Node(8)
  val node4 = Node(5)
  val node5 = Node(10)
  val node6 = Node(2)
  val node7 = Node(1)
  val node8 = Node(9)

  node1.next = Some(node2)
  node2.next = Some(node3)
  node3.next = Some(node4)
  node4.next = Some(node5)
  node5.next = Some(node6)
  node6.next = Some(node7)
  //node7.next = Some(node8)


  val asdas = partition(node1, 5)


  val ttttt = ""


}
