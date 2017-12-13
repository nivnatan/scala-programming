package exercises

import exercises.StackAndQueues.MyQueue

/**
  * Created by nivnatan on 12/8/2017.
  */
object TreesAndGraphs extends App {

  case class TreeNode[T](data: T, var left: Option[TreeNode[T]] = None, var right: Option[TreeNode[T]] = None)

  val node1 = TreeNode(10)
  val node2 = TreeNode(5)
  val node3 = TreeNode(20)
  val node4 = TreeNode(25)
  val node5 = TreeNode(15)
  val node6 = TreeNode(2)
  val node7 = TreeNode(6)

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

  /**
    * For each node, set the sumRoute to be the sum of all nodes visited during the route to that node
    */
  object TreeNodesRouteSum {
    case class TreeNodeRouteSum(data: Int, var left: Option[TreeNodeRouteSum] = None, var right: Option[TreeNodeRouteSum] = None, var sumRoute: Int = 0)

    def routeSum(root: TreeNodeRouteSum) = {
      def routeSumRec(root: TreeNodeRouteSum, routeSum: Int): Unit = {
        root.sumRoute = root.data + routeSum
        root.left.map(routeSumRec(_, root.sumRoute))
        root.right.map(routeSumRec(_, root.sumRoute))
      }
      routeSumRec(root, 0)
    }
  }

  /**
    * Create a linked list out of the tree nodes as if they were to appear during in-inorder traversal
    * The list should not be returned, but rather be updated using the *next variable
    * The idea - first we need to find the head of the list which is the leftmost leaf.
    * then we iterate using the inorder traversal template, and each time, we reach the tail of the list and append the new node
    * For example -
    *              A
    *           /    \
    *         /       \
    *        B         C
    *
    * B(next) -> A(next) -> C
    */
  object TreeNodeLinkedList {
    case class TreeNodeLinkedList[T](data: T, var left: Option[TreeNodeLinkedList[T]] = None, var right: Option[TreeNodeLinkedList[T]] = None, var next: Option[TreeNodeLinkedList[T]] = None) {
      override def toString = s"currentNode=${data.toString}, listNext=${next}"
    }

    def tree2List[T](root: TreeNodeLinkedList[T]): TreeNodeLinkedList[T] = {
      def tree2ListRec(root: TreeNodeLinkedList[T], headOfList: TreeNodeLinkedList[T]): Unit = {
        root.left.map(tree2ListRec(_,headOfList))
        if(root != headOfList) {
          var tailList = headOfList
          while(tailList.next.isDefined) tailList = tailList.next.get
          tailList.next = Some(root)
        }
        root.right.map(tree2ListRec(_,headOfList))
      }
      def initHeadOfListWhichIsTheLeftMostLeaf(root: TreeNodeLinkedList[T]): TreeNodeLinkedList[T] = root.left.map(initHeadOfListWhichIsTheLeftMostLeaf).getOrElse(root)
      val headOfList = initHeadOfListWhichIsTheLeftMostLeaf(root)
      tree2ListRec(root, headOfList)
      headOfList
    }
  }

  /**
    * Given a value, search for a node that matches the value and return it
    *
    * @param root
    * @return node that matches the searched value
    */
  def findNode[T](root: TreeNode[T], value: T): Option[TreeNode[T]] = {
    if(root.data == value) Some(root)
    else (root.left.flatMap(findNode(_, value)) orElse root.right.flatMap(findNode(_, value)))
  }

  /**
    * Given a value, search for a node in a binary search tree that matches the value and return it
    *
    * @param root
    * @return node that matches the searched value
    */
  def binTreeSearch(root: TreeNode[Int], value: Int): Option[TreeNode[Int]] = {
    if(root.data == value)     Some(root)
    else if(value < root.data) root.left.flatMap(findNode(_, value))
    else                       root.right.flatMap(findNode(_, value))
  }

  /**
    * Implement a function to check if a tree is balanced. For the purposes of this question,
    * a balanced tree is defined to be a tree such that no two leaf nodes differ in distance
    * from the root by more than one.
    *
    * @param root
    * @return true if the tree is balanced, false otherwise
    */
  def isTreeBalanced(root: TreeNode[Int]): Boolean = {
    val leftHeight  = root.left.map(treeHeightMoreElegant).getOrElse(0)
    val rightHeight = root.right.map(treeHeightMoreElegant).getOrElse(0)
    Math.abs(leftHeight - rightHeight) <= 1 && root.left.map(isTreeBalanced).getOrElse(true) && root.right.map(isTreeBalanced).getOrElse(true)
  }

  /**
    * Given a sorted (increasing order) array, write an algorithm to create a binary tree with
    * minimal height.
    *
    * @param arr
    * @return binary tree
    */
  def binaryTreeFromArray(arr: Array[Int]): Option[TreeNode[Int]] = {
    def binaryTreeFromArrayRec(min: Int, max:Int): Option[TreeNode[Int]] = {
      if(min > max) None
      else {
        val mid = (max + min) / 2
        val newNode = TreeNode(arr(mid))
        newNode.left = binaryTreeFromArrayRec(min, mid - 1)
        newNode.right = binaryTreeFromArrayRec(mid + 1, max)
        Some(newNode)
      }
    }
    binaryTreeFromArrayRec(0, arr.length-1)
  }

  def levelOrderTraversalUsingQueue(root: TreeNode[Int]): Unit = {
    val queue = new MyQueue[TreeNode[Int]]
    queue.add(root)
    while(!queue.isEmpty) {
      val element = queue.remove
      element.left.map(queue.add)
      element.right.map(queue.add)
    }
  }

  /**
    * Print all nodes at the same level in each new line
    * For example:
    * 10
    * 5 20
    * 2 6 15 25
    * Complexity - O(n^2)
    *
    * @param root
    * @return
    */
  def levelOrderPrint(root: TreeNode[Int]): Unit = {
    def printLevel(root: TreeNode[Int], levelToPrint: Int, currentLevel: Int): Unit = {
      if(currentLevel == levelToPrint) print(root.data + " ")
      root.left.map(printLevel(_,levelToPrint, currentLevel + 1))
      root.right.map(printLevel(_,levelToPrint, currentLevel + 1))
    }

    val height = treeHeight(root) // calculate the height first
    for(i <- 0 until height) {    // for every level in the tree, scan preOrder the tree and print only the nodes at the desired level
      printLevel(root, i, 0)
      print("\n")
    }
  }

  /**
    * Same as levelOrderPrint, but using a better complexity of O(n)
    *
    * @param root
    * @return
    */
  def levelOrderPrintUsingQueue(root: TreeNode[Int]): Unit = {
    val queue = new MyQueue[TreeNode[Int]]
    queue.add(root)
    while(!queue.isEmpty) {
      for(_ <- 0 until queue.size) {
        val head = queue.remove
        print(head.data + " ")
        head.left.map(queue.add)
        head.right.map(queue.add)
      }
      print("\n")
    }
  }

  /**
    * Given two binary trees, check if the first tree is subtree of the second one. A subtree of a tree T is a tree S consisting of a node in T and all of its descendants in T.
    *
    * @param root1
    * @param root2
    * @return true if root2 is a subtree of root1
    */
  def isSubTree(root1: TreeNode[Int], root2: TreeNode[Int]): Boolean = {

    def areTreesIdentical(root1: TreeNode[Int], root2: TreeNode[Int]): Boolean = {
      if(root1.data != root2.data) false
      else {
        (root1.left, root2.left, root1.right, root2.right) match {
          case (None, None, None, None)                                   => true
          case (Some(r1Left), Some(r2Left), Some(r1Right), Some(r2Right)) => areTreesIdentical(r1Left, r2Left) && areTreesIdentical(r1Right, r2Right)
          case _                                                          => false
        }
      }
    }

    if(root1.data == root2.data && areTreesIdentical(root1, root2)) true
    else {
      root1.left.map(isSubTree(_, root2)).getOrElse(false) ||  root1.right.map(isSubTree(_, root2)).getOrElse(false)
    }
  }
}