package exercises

import exercises.StackAndQueues.MyQueue

/**
  * Created by nivnatan on 12/8/2017.
  */
object TreesAndGraphs extends App {

  case class TreeNode[T](data: T,
                         var left: Option[TreeNode[T]] = None,
                         var right: Option[TreeNode[T]] = None,
                         var successor: Option[TreeNode[T]] = None,
                         var parent: Option[TreeNode[T]] = None,
                         var isLocked: Boolean = false) {
    def isLeaf: Boolean = left.isEmpty && right.isEmpty
  }

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
    *             10
    *           /    \
    *         /       \
    *        5         20
    *      /  \       / \
    *    /     \    /    \
    *   2       6  15    25
    */

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
    * D -> E -> B -> F -> G -> C -> A (Bottom up)
    */
  def postOrder[T](root: TreeNode[T]): Unit = {
    root.left.map(postOrder)
    root.right.map(postOrder)
    println(root.data)
  }

  /**
    * Find the max element in the tree
    *
    * @param root
    * @return max element in tree
    */
  def maxElementInTree(root: TreeNode[Int]): Int = {
    val leftMax = Math.max(root.left.map(maxElementInTree).getOrElse(root.data), root.data)
    val rightMax = Math.max(root.right.map(maxElementInTree).getOrElse(root.data), root.data)
    Math.max(leftMax, rightMax)
  }

  /**
    * Validate if a tree is a binary search tree
    * Solution - perform inorder traversal which should traverse a BST in an acceding manner - if one of the nodes is smaller then the previous one - resturn false
    *
    * @param root
    * @return true if the tree is BST, false otherwise
    */
  def validateBST(root: TreeNode[Int]): Boolean = {
    var prev: Option[TreeNode[Int]] = None
    def validateBSTRec(root: TreeNode[Int]): Boolean = {
      root.left.map(validateBSTRec(_)).getOrElse(true)
      if(prev.map(_.data > root.data).getOrElse(false)) false
      else {
        prev = Some(root)
        root.right.map(validateBSTRec(_)).getOrElse(true)
      }
    }
    validateBSTRec(root)
  }

  /**
    * Validate if a tree is a binary search tree
    * Solution - perform inorder traversal which should traverse a BST in an acceding manner - if one of the nodes is smaller then the previous one - resturn false
    *
    * @param root
    * @return true if the tree is BST, false otherwise
    */
  def validateBSTSimple(root: TreeNode[Int]): Boolean = {
    def validateBSTSimpleRec(root: TreeNode[Int], min: Int, max: Int): Boolean = {
      if(root.data < min || root.data > max) false
      else root.left.map(validateBSTSimpleRec(_, min, root.data-1)).getOrElse(true) && root.right.map(validateBSTSimpleRec(_, root.data+1, max)).getOrElse(true)
    }
    validateBSTSimpleRec(root, Integer.MIN_VALUE, Integer.MAX_VALUE)
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
    1 + Math.max(treeHeightRec(root, 1, true), treeHeightRec(root, 1, false))
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
    *
    * @param root
    * @return
    */
  def levelOrderPrint[T](root: TreeNode[T]): Unit = {
    def printLevel(root: TreeNode[T], levelToPrint: Int, currentLevel: Int): Unit = {
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

    // Check for a given two roots, if the trees are identical
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

    // Another solution could be suggested as follows:
    //Tree S is a subtree of T if both inorder and preorder traversals of S arew substrings of inorder and preorder traversals of T respectively.
    //1) Find inorder and preorder traversals of T, store them in two auxiliary arrays inT[] and preT[].
    // 2) Find inorder and preorder traversals of S, store them in two auxiliary arrays inS[] and preS[].
    // 3) If inS[] is a subarray of inT[] and preS[] is a subarray preT[], then S is a subtree of T. Else not.
  }

  /**
    * Print the path from the root to a given node
    * Algorithm:
    * 1. Start from the root and com­pare it with x, if matched then we have found the node.
    * 2. Else go left and right.
    * 3. Recursively do step 2 and 3 till you find the node x.
    * 4. Now when you have found the node, stop the recursion.
    * 5. Now while going back to the root while back track­ing, store the node val­ues in the ArrayList.
    * 6. Reverse the ArrayList and print it.
    *
    * @param root
    * @param node1
    * @return
    */
  def printPathToAGivenNode(root: TreeNode[Int], node1: Int): Unit = {
    val arrayResult = scala.collection.mutable.ArrayBuffer.empty[TreeNode[Int]]
    def printPathToAGivenNodeRec(root: TreeNode[Int], node1: Int): Boolean = {
      if(root.data == node1 ||
        root.left.map(printPathToAGivenNodeRec(_,node1)).getOrElse(false) ||
        root.right.map(printPathToAGivenNodeRec(_,node1)).getOrElse(false)) {
        arrayResult += root
        true
      } else {
        false
      }
    }
    printPathToAGivenNodeRec(root, node1)
    arrayResult.reverse.foreach(d => print(d.data + " "))
  }

  /**
    * Find the lowest common ancestor in a binary search tree for two given nodes (assuming they are known to be part of the tree and node1 < node2)
    *
    * @param root
    * @param node1
    * @param node2
    * @return the lowest common ancestor in a binary search tree
    */
  def findLCAInBST(root: TreeNode[Int], node1: Int, node2: Int): Option[TreeNode[Int]] = {
    if(node1 <= root.data && node2 >= root.data) Some(root)
    else if(node1 <= root.data && node2 <= root.data) root.left.map(findLCAInBST(_, node1, node2)).getOrElse(None)
    else root.right.map(findLCAInBST(_, node1, node2)).getOrElse(None)
  }

  /**
    * Print right side of a BST tree
    *
    * @param root
    */
  def printRightSideOfBSTTree(root: TreeNode[Int]): Unit = {
    var maximumLevelVisited = 0
    // The idea is to traverse the tree level order starting from right, and update a global variable maximumLevelVisited to know that it's the first element in every level
    def printRightSideOfBSTTree(root: TreeNode[Int], currentLevel: Int): Unit = {

      if(maximumLevelVisited < currentLevel) {
        println(s"data=${root.data}")
        maximumLevelVisited = currentLevel
      }

      root.right.map (printRightSideOfBSTTree(_, currentLevel + 1))
      root.left.map(printRightSideOfBSTTree(_, currentLevel + 1))
    }
    printRightSideOfBSTTree(root, 1)
  }

  /**
    * Print right side of a BST tree using queue
    *
    * @param root
    */
  def printRightSideOfBSTTreeUsingQueue(root: TreeNode[Int]): Unit = {
    val queue = new MyQueue[TreeNode[Int]]
    queue.add(root)
    while(!queue.isEmpty) {
      for(i <- 0 until queue.size) {
        val head = queue.remove
        if(i == 0) println(head.data)
        head.right.map(queue.add)
        head.left.map(queue.add)
      }
    }
  }

  /**
    * Insert new node to a BST tree
    *
    * @param root
    * @return root
    */
  def insertNewNodeToBinaryTree(root: TreeNode[Int], value: Int): TreeNode[Int] = {
    def insertNewNodeToBinaryTreeRec(root: TreeNode[Int]): Unit = {
      if(value > root.data) {
        root.right.map(insertNewNodeToBinaryTreeRec).getOrElse(root.right = Some(TreeNode(value)))
      } else {
        root.left.map(insertNewNodeToBinaryTreeRec).getOrElse(root.left = Some(TreeNode(value)))
      }
    }
    insertNewNodeToBinaryTreeRec(root)
    root
  }


  /**
    * Remove nodes from BST that have keys outside the valid range
    * Example -
    *
    *             10
    *           /    \
    *         /       \
    *        5         20
    *      /  \       / \
    *    /     \    /    \
    *   2       6  15    25
    *
    * truncate [4,8]
    *
    * 5
    *  \
    *   \
    *    6
    *
    * @param root
    * @return updated root according to the requirements
    */
  def truncateNodesFromBSTAccordingToRange(root: TreeNode[Int], min: Int, max: Int): Option[TreeNode[Int]] = {
    def truncateNodesFromBSTAccordingToRangeRec(root: TreeNode[Int]): Option[TreeNode[Int]] = {
      // traverse bottom up (post order)
      // first fix the left and right subtrees of root
      root.left  = root.left.flatMap(truncateNodesFromBSTAccordingToRangeRec)
      root.right = root.right.flatMap(truncateNodesFromBSTAccordingToRangeRec)
      // now fix the root.  There are 2 possible cases for toot
      // 1.a) Root's key is smaller than min value (root is not in range)
      if(root.data < min) {
        root.right
      }
      // 1.b) Root's key is greater than max value (root is not in range)
      else if(root.data > max) {
        root.left
      }
      // 2. Root is in range
      else {
        Some(root)
      }
    }
    truncateNodesFromBSTAccordingToRangeRec(root)
  }

  /**
    * Maximum width of a binary tree
    * Example -
    *
    *             10
    *           /    \
    *         /       \
    *        5         20
    *      /  \       / \
    *    /     \    /    \
    *   2       6  15    25
    *
    * maximum width = 4
    *
    * @param root
    * @return maximum width
    */
  def binaryTreeMaximumWidth(root: TreeNode[Int]): Int = {
    val queue = new MyQueue[TreeNode[Int]]
    queue.add(root)
    var maximumWidth = 0
    while(!queue.isEmpty) {
      if(queue.size > maximumWidth) {
        maximumWidth = queue.size
      }
      for(_ <- 0 until queue.size) {
        val node = queue.remove
        node.left.map(queue.add)
        node.right.map(queue.add)
      }
    }
    maximumWidth
  }

  /**
    * Print leaf to root path for a given leaf
    *
    * @param root
    * @return root
    */
  def printPathFromLeafToRoot(root: TreeNode[Int], leafData: Int) = {
    def printPathFromLeafToRootRec(root: TreeNode[Int]): Boolean = {
      if(root.left.map(printPathFromLeafToRootRec).getOrElse(false) || root.right.map(printPathFromLeafToRootRec).getOrElse(false)) {
        println(root.data)
        true
      } else if(root.data == leafData){
        println(root.data)
        true
      } else false
    }
    printPathFromLeafToRootRec(root)
  }

  /**
    * Construct Tree from given Inorder and Preorder traversals
    * For example -
    * Inorder sequence: D B E A F C
    * Preorder sequence: A B D E C F
    *
    * Explanation -
    * In a Preorder sequence, leftmost element is the root of the tree. So we know ‘A’ is root for given sequences.
    * By searching ‘A’ in Inorder sequence, we can find out all elements on left side of ‘A’ are in left subtree and elements on right are in right subtree. So we know below structure now.
    *
    *         A
    *       /   \
    *      /     \
    *     B       C
    *    / \     /
    *   /   \   /
    *  D     E F
    *
    * @param inOrderSeq
    * @param preOrderSeq
    * @return root of the new constructed node
    */
  def constructTreeFromInOrderAndPreOrderTraversals(inOrderSeq: Seq[String], preOrderSeq: Seq[String]): Option[TreeNode[String]] = {
    def constructTreeFromInOrderAndPreOrderTraversalsRec(inOrderSeq: Seq[String], preOrderIndex: Int): Option[TreeNode[String]] = {
      inOrderSeq match {
        case Seq()         => None
        case head +: Seq() => Some(TreeNode(head))
        case seq           => {
          val root               = preOrderSeq(preOrderIndex) // preOrderSeq first element is the root
          val rootNode           = TreeNode(root) // create a new tree node
          val rootIndexInInorder = seq.indexOf(root) // find the root in the inOrderSeq and then everything to the left is the left side sub tree, and to the right is the right side sub tree
          val inOrderLeftSeq     = seq.take(rootIndexInInorder) //take left side of the seq until the root
          val inOrderRightSeq    = seq.drop(rootIndexInInorder + 1) //take right side of the seq from the root
          // rec calls
          rootNode.left          = constructTreeFromInOrderAndPreOrderTraversalsRec(inOrderLeftSeq, preOrderIndex + 1)
          rootNode.right         = constructTreeFromInOrderAndPreOrderTraversalsRec(inOrderRightSeq, preOrderIndex + 1 + inOrderLeftSeq.size)

          Some(rootNode)
        }
      }
    }
    constructTreeFromInOrderAndPreOrderTraversalsRec(inOrderSeq, 0)
  }

  /**
    * Construct BST from given a Preorder traversals
    * For example -
    * Preorder sequence: 9, 5, 3, 4, 7, 12, 11, 14
    *
    * Explanation -
    * First number in the preorder has to be the root of the BST always.
    * So that becomes the root of the solution tree.
    * Next, preorder traversal prints the left subtree first and then the right subtree.
    * Right subtree has greater elements than root and left subtree has smaller.
    * So, if we find the index 'k' of first element greater than the root, then left subtree could be formed from elements 0 to 'k-1' and right subtree can be formed from elements 'k' to end.
    * Finding the 'k' at every substep needs O(n) time, so total order of the solution is O(n2)
    *
    *        9
    *      /  \
    *     /    \
    *    5      12
    *   / \    /  \
    *  3   7  11  14
    *   \
    *    4
    *
    * @param preOrderSeq
    * @return root of the new constructed node
    */
  def constructTreeFromPreOrderTraversal(preOrderSeq: Seq[Int]): Option[TreeNode[Int]] = {
    def constructTreeFromPreOrderTraversalRec(preOrderSeq: Seq[Int]): Option[TreeNode[Int]] = {
      preOrderSeq match {
        case Seq()         => None
        case head +: Seq() => Some(TreeNode(head))
        case seq           => {
          val root               = seq.head // preOrderSeq first element is the root
          val rootNode           = TreeNode(root) // create a new tree node
          val (preOrderLeftSeq, preOrderRightSeq) = seq.tail.partition(_ <= root)
          // rec calls
          rootNode.left          = constructTreeFromPreOrderTraversalRec(preOrderLeftSeq)
          rootNode.right         = constructTreeFromPreOrderTraversalRec(preOrderRightSeq)
          Some(rootNode)
        }
      }
    }
    constructTreeFromPreOrderTraversalRec(preOrderSeq)
  }

  /**
    * Find if sum of two nodes is equal to a given number in BST
    *
    * @param root
    * @param sum
    * @return true / false
    */
  def pairSumInBST(root: TreeNode[Int], sum: Int): Boolean = {
    // perform inorder and save in array (will create sorted array)
    // then perform - find pait that equal to a given sum using hashmap or increase decrease two pointers
    true
  }

  /**
    * Given a Binary tree and a sum S, print all the paths, starting from root, that sums upto the given sum.
    * Example -
    * Input : sum = 38,
    *          Root
    *           10
    *        /     \
    *      28       13
    *            /     \
    *          14       15
    *         /   \     /  \
    *        21   22   23   24
    * Output : Path found: 10 28
    * Path found: 10 13 15
    *
    * @param root
    * @param sum
    */
  def printAllPathsFromRootUptoGivenSum(root: TreeNode[Int], sum: Int): Unit = {
    // solution - traverse preOrder and save the elements in a list and update sum.
    def printAllPathsFromRootUptoGivenSumRec(root: TreeNode[Int], sum: Int, visitList: List[TreeNode[Int]]): Unit = {
      val updatedList = visitList :+ root
      if(root.data == sum) {
        println((updatedList.map(_.data)).mkString(" -> "))
      } else {
        root.left.map(printAllPathsFromRootUptoGivenSumRec(_, sum - root.data, updatedList))
        root.right.map(printAllPathsFromRootUptoGivenSumRec(_, sum - root.data, updatedList))
      }
    }
    printAllPathsFromRootUptoGivenSumRec(root, sum, Nil)
  }

  def printAllPathsFromAnyNodeUptoGivenSum(root: TreeNode[Int], sum: Int): Unit = {
    def printAllPathsFromAnyNodeUptoGivenSumRec(root: TreeNode[Int]): Unit = {
      printAllPathsFromRootUptoGivenSum(root, sum)
      root.left.map(printAllPathsFromRootUptoGivenSum(_, sum))
      root.right.map(printAllPathsFromRootUptoGivenSum(_, sum))
    }
    printAllPathsFromAnyNodeUptoGivenSumRec(root)
  }

  /**
    * Given 2 nodes in a tree, find common ancestor
    *
    * @param root
    * @param node1
    * @param node2
    * @return common ancestor if exists
    */
  def commonAncestor(root: TreeNode[Int], node1: Int, node2: Int): Option[Int] = {
    def commonAncestorRec(root: TreeNode[Int], node: Int): Boolean = {
      if (root.data == node) true
      else (root.left.map(commonAncestorRec(_, node)).getOrElse(false) || root.right.map(commonAncestorRec(_, node)).getOrElse(false))
    }

    if(root.left.map(commonAncestorRec(_, node1)).getOrElse(false) && root.right.map(commonAncestorRec(_, node2)).getOrElse(false)) {
      Some(root.data)
    } else if(root.left.map(commonAncestorRec(_, node2)).getOrElse(false) && root.right.map(commonAncestorRec(_, node1)).getOrElse(false)) {
      Some(root.data)
    } else root.left.flatMap(commonAncestor(_, node1, node2)) orElse root.right.flatMap(commonAncestor(_, node1, node2))
  }

  /**
    * A binary tree is foldable if it's a mirror image of itself
    * i.e. if its left and right subtrees are identical mirror images
    *
    * @param root
    * @return true if root is foldable, false otherwise
    */
  def isFoldableTree(root: TreeNode[Int]): Boolean = {
    def isFoldableTreeRec(root1: TreeNode[Int], root2: TreeNode[Int]): Boolean = {
      if(root1.data != root2.data) false
      else {
        (root1.left, root1.right, root2.left, root2.right) match {
          case (None, None, None, None) => true
          case (Some(r1L), Some(r1R), Some(r2L), Some(r2R)) => isFoldableTreeRec(r1L, r2R) && isFoldableTreeRec(r1R, r2L)
          case _ => false
        }
      }
    }
    (root.left, root.right) match {
      case (Some(r1), Some(r2)) => isFoldableTreeRec(r1,r2)
      case (None,None)          => true
      case _                    => false
    }
  }

  /**
    * Populate In-Order successor in each node
    * Successor means next element obtained when we traverse.
    * Example -
    *             10
    *           /    \
    *         /       \
    *        5         20
    *      /  \       / \
    *    /     \    /    \
    *   2       6  15    25
    *
    *  In the above diagram, inorder successor of node 2 is 5 and node 6 is 10.
    *
    * @param root
    */
  def inOrderSuccessor(root: TreeNode[Int]): Unit = {
    //if we keep a pointer to previously visited node, then current node is the successor.
    var globalPrev: Option[TreeNode[Int]] = None
    def inOrderSuccessorRec(root: TreeNode[Int]): Unit = {
      root.left.map(inOrderSuccessorRec)
      globalPrev.map(_.successor = Some(root))
      globalPrev = Some(root)
      root.right.map(inOrderSuccessorRec)
    }
    inOrderSuccessorRec(root)
  }

  /**
    * Given a binary tree, write a function to get the maximum width of the given tree. Width of a tree is maximum of widths of all levels.
    * Example -
    *          1
    *         /  \
    *        2    3
    *      /  \     \
    *     4    5     8
    *         /  \
    *        6    7
    * The maximum width of the tree is 3
    *
    * @param root
    * @return maximum width of the tree
    */
  def maximumWidthOfABinaryTree(root: TreeNode[Int]): Int = {

    // this solution is not optimal, solution using level order traversal using queue is better and can be found far above in this code sheet

    def height(root: TreeNode[Int]): Int = {
      val leftH  = root.left.map(height).getOrElse(0)
      val rightH = root.left.map(height).getOrElse(0)
      1 + Math.max(leftH, rightH)
    }

    def countWidthPerLevelImmutuble(root: TreeNode[Int], levelToCount: Int): Int = {
      if(levelToCount == 1) 1
      else root.left.map(countWidthPerLevelImmutuble(_, levelToCount - 1)).getOrElse(0) + root.right.map(countWidthPerLevelImmutuble(_, levelToCount - 1)).getOrElse(0)
    }

    def countWidthPerLevel(root: TreeNode[Int], levelToCount: Int) = {
      var sum = 0
      def countWidthPerLevelRec(root: TreeNode[Int], currentLevel: Int, levelToCount: Int): Unit = {
        if(currentLevel == levelToCount) sum+=1
        root.left.map(countWidthPerLevelRec(_, currentLevel + 1, levelToCount))
        root.right.map(countWidthPerLevelRec(_, currentLevel + 1, levelToCount))
      }
      countWidthPerLevelRec(root, 1, levelToCount)
      sum
    }

    def iterateLevels(root: TreeNode[Int]): Int = {
      val treeHeight: Int = height(root)
      var maximumWidth = 0
      for(i <- 1 to treeHeight) {
        val levelWidth = countWidthPerLevelImmutuble(root, i)
        if(levelWidth > maximumWidth) {
          maximumWidth = levelWidth
        }
      }
      maximumWidth
    }

    maximumWidthOfABinaryTree(root)
  }

  /**
    * Given a BST tree and valid range of keys. remove nodes from BST that have keys outside the valid range
    *
    *
    *              10
    *            /    \
    *          /       \
    *         5         20
    *       /  \       / \
    *     /     \    /    \
    *    2       6  15    25
    * Example with range (4,8):
    *
    *
    *
    *
    *        5
    *         \
    *          \
    *           6
    *
    * @param root
    * @return maximum width of the tree
    */
    def removeNodesFromBSTOutsideValidRange(root: TreeNode[Int], range:(Int,Int)): Option[TreeNode[Int]] = {
      root.left = root.left.map(removeNodesFromBSTOutsideValidRange(_, range)).getOrElse(None)
      root.right = root.right.map(removeNodesFromBSTOutsideValidRange(_, range)).getOrElse(None)

      //the idea is to traverse the tree bottom-up fashion and truncate left and right subtree first before processing a node.
      //if its key fall within the valid range, nothing needs to be done.
      //if root's key is smaller than the minimum allowed, we remove it and set root to root's right child.
      //if root's key is larger than the maximum allowed, we remove it and set root to root's left child.
      if (root.data < range._1) {
        root.right
      } else if (root.data > range._2) {
        root.left
      } else {
        Some(root)
      }
    }

  /**
    *
    * Implement locking in a binary tree. A binary tree node can be locked or unlocked only if all of its descendants or ancestors are not locked.
    * Design a binary tree node class with the following methods:
    * is_locked, which returns whether the node is locked
    * lock, which attempts to lock the node. If it cannot be locked, then it should return false. Otherwise, it should lock it and return true.
    * unlock, which unlocks the node. If it cannot be unlocked, then it should return false. Otherwise, it should unlock it and return true.
    */
  object LockingBinaryTree {

    def isLocked(node: TreeNode[Int]): Boolean = node.isLocked

    def lock(node: TreeNode[Int]): Boolean = {
      if(canNodeBeLocked(node)) {
        node.isLocked = true
        true
      } else {
        false
      }
    }

    def unLock(node: TreeNode[Int]): Boolean = {
      if(canNodeBeLocked(node)) {
        node.isLocked = false
        true
      } else {
        false
      }
    }

    private def canNodeBeLocked(node: TreeNode[Int]): Boolean = {
      // check children
      if(!node.left.forall(checkChildLock)) false
      else if(!node.right.forall(checkChildLock)) false
      // check parents
      else node.parent.forall(checkParentLock)
    }

    private def checkParentLock(node: TreeNode[Int]): Boolean = {
      if(node.isLocked) false
      else node.parent.forall(checkParentLock)
    }

    private def checkChildLock(node: TreeNode[Int]): Boolean = {
      if(node.isLocked) false
      else if(!node.left.forall(checkChildLock)) false
      else node.right.forall(checkChildLock)
    }
  }

  /**
    * Given the root to a binary search tree, find the second largest node in the tree.
    * reference - https://medium.com/@johnathanchen/find-the-2nd-largest-element-in-a-binary-search-tree-interview-question-f566c52188a1
    */
  def secondLargestNode(root: TreeNode[Int]): Int = {
    // because this is binary search tree, the largest is the most right element
    def findLargest(root: TreeNode[Int]): Int = {
      root.right.map(findLargest).getOrElse(root.data)
    }

    def findSecondLargest(root: TreeNode[Int]): Int = {
      (root.left, root.right) match {
        // case 1: current node is the parent of the last right node which means it's the second largets node
        case (_, Some(right)) if right.left.isEmpty && right.right.isEmpty => root.data
        // case 2: current node is the last right node of the tree which means it's the largest node, so just need to find the largest node on its left subtree
        case (Some(left), None) => findLargest(left)
        // otherwise we iterate to the right. if there is no right and no left it's basically an error
        case _ => findSecondLargest(root.right.get)
      }
    }

    findSecondLargest(root)
  }
}
