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

  val node1 = Node(1)
  val node2 = Node(1)
  val node3 = Node(3)
  val node4 = Node(2)
  val node5 = Node(5)
  val node6 = Node(6)
  val node7 = Node(7)
  val node8 = Node(8)

  node1.next = Some(node2)
  node2.next = Some(node3)
  node3.next = Some(node4)
  node4.next = Some(node5)
  node5.next = Some(node6)
  node6.next = Some(node7)
  node7.next = Some(node8)


  val asdas = distinct2Pointers(node1)


  val ttttt = ""


}
