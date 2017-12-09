/**
  * Created by nivnatan on 12/8/2017.
  */
object TreesAndGraphs extends App {

  case class TreeNode[T](data: T, var left: Option[TreeNode[T]] = None, var right: Option[TreeNode[T]] = None)

  val node1 = TreeNode("A")
  val node2 = TreeNode("B")
  val node3 = TreeNode("C")
  val node4 = TreeNode("G")
  val node5 = TreeNode("F")
  val node6 = TreeNode("D")
  val node7 = TreeNode("E")

  node1.left  = Some(node2) // B
  node1.right = Some(node3) // C
  node2.left  = Some(node6) // D
  node2.right = Some(node7) // E
  node3.left  = Some(node5) // F
  node3.right = Some(node4) // G

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
}
