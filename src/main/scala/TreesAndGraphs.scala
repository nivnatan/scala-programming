/**
  * Created by nivnatan on 12/8/2017.
  */
object TreesAndGraphs extends App {

  case class TreeNode[T](data: T, var left: Option[TreeNode[T]] = None, var right: Option[TreeNode[T]] = None)

  val node1 = TreeNodeLinkedList(10)
  val node2 = TreeNodeLinkedList(5)
  val node3 = TreeNodeLinkedList(20)
  val node4 = TreeNodeLinkedList(25)
  val node5 = TreeNodeLinkedList(15)
  val node6 = TreeNodeLinkedList(2)
  val node7 = TreeNodeLinkedList(6)

  node1.left  = Some(node2) // B
  node1.right = Some(node3) // C
//  node2.left  = Some(node6) // D
//  node2.right = Some(node7) // E
//  node3.left  = Some(node5) // F
//  node3.right = Some(node4) // G

  /**
    *              A
    *           /    \
    *         /       \
    *        B         C
    *      /  \       / \
    *    /     \    /    \
    *   D       E  F      G
    */

  /**
    * D -> B -> E -> A -> F -> C -> G
    */
  def inOrder[T](root: TreeNode[T]): Unit = {
    root.left.map(inOrder)
    println(root.data)
    root.right.map(inOrder)
  }

  /**
    * A -> B -> D -> E -> C -> F -> G
    */
  def preOrder[T](root: TreeNode[T]): Unit = {
    println(root.data)
    root.left.map(preOrder)
    root.right.map(preOrder)
  }

  /**
    * D -> E -> B -> F -> G -> C -> A
    */
  def postOrder[T](root: TreeNode[T]): Unit = {
    root.left.map(postOrder)
    root.right.map(postOrder)
    println(root.data)
  }

  /**
    * Find tree height (number of nodes from the root until the farthest leaf)
    * e.g. Input : 23, 34,56,21,21,56,78,23, 34
    * Output: 23
    *
    * @param root
    * @return number which is not repeated in the array of integers
    */
  def treeHeight(root: TreeNode[_]) = {
    def treeHeightRec(root: TreeNode[_], numberOfNodesVisited: Int, isRight: Boolean): Int = {
      if(isRight) root.left.map(treeHeightRec(_, numberOfNodesVisited + 1, isRight)).getOrElse(numberOfNodesVisited)
      else root.right.map(treeHeightRec(_, numberOfNodesVisited + 1, isRight)).getOrElse(numberOfNodesVisited)
    }
    Math.max(treeHeightRec(root, 1, true), treeHeightRec(root, 1, false))
  }

  def treeHeightMoreElegant(root: TreeNode[_]): Int = {
    1 + Math.max(root.left.map(treeHeightMoreElegant).getOrElse(0), root.right.map(treeHeightMoreElegant).getOrElse(0))
  }

  case class TreeNodeRouteSum(data: Int, var left: Option[TreeNodeRouteSum] = None, var right: Option[TreeNodeRouteSum] = None, var sumRoute: Int = 0)
  /**
    * Find tree height (number of nodes from the root until the farthest leaf)
    * e.g. Input : 23, 34,56,21,21,56,78,23, 34
    * Output: 23
    *
    * @param root
    * @return number which is not repeated in the array of integers
    */
  def routeSum(root: TreeNodeRouteSum) = {
    def routeSumRec(root: TreeNodeRouteSum, routeSum: Int): Unit = {
      root.sumRoute = root.data + routeSum
      root.left.map(routeSumRec(_, root.sumRoute))
      root.right.map(routeSumRec(_, root.sumRoute))
    }
    routeSumRec(root, 0)
  }

  case class TreeNodeLinkedList[T](data: T, var left: Option[TreeNodeLinkedList[T]] = None, var right: Option[TreeNodeLinkedList[T]] = None, var next: Option[TreeNodeLinkedList[T]] = None) {
    override def toString = ""
  }
  /**
    * Find tree height (number of nodes from the root until the farthest leaf)
    * e.g. Input : 23, 34,56,21,21,56,78,23, 34
    * Output: 23
    * https://cboard.cprogramming.com/c-programming/108712-changing-tree-into-list-question.html
    * https://www.cs.bgu.ac.il/~prog141/wiki.files/prog141_ps11.pdf
    * @param root
    * @return number which is not repeated in the array of integers
    */
  def tree2List[Int](root: TreeNodeLinkedList[Int]): Unit = {
    def tree2ListRec(root: TreeNodeLinkedList[Int]): Option[TreeNodeLinkedList[Int]] = {
      root.next = root.right.map(tree2ListRec(_)).getOrElse(None)

      root.left.map(tree2ListRec(_, Some(root)))
      root.next = prev
      root.right.map(tree2ListRec(_, Some(root)))
    }
    tree2ListRec(root, None)
  }

  val asdas = tree2List(node1)
  val adsa = "erh"
}
