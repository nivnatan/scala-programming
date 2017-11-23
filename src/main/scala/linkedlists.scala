import scala.annotation.tailrec

/**
  * Created by niv on 23/11/2017.
  */
object linkedlists {

  case class Node(data: Int, var next: Option[Node]) {
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
}
