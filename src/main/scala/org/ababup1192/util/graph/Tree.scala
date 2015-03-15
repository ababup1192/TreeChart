package org.ababup1192.util.graph

import scala.collection.mutable.{ListBuffer, OpenHashMap => HashMap}
import scala.language.postfixOps

sealed trait TreeNode

case object RootNode extends TreeNode

case class ElementNode[T](value: T) extends TreeNode

/**
 * Tree data structure
 * @tparam T TreeNode type
 */
class Tree[T] {

  val rootNode = RootNode
  val tree = HashMap[TreeNode, ListBuffer[TreeNode]](rootNode -> ListBuffer())

  protected[this] val positionByNode = HashMap[TreeNode, NodePosition](rootNode -> NodePosition.ROOT)
  protected[this] val nodeByPosition = positionByNode.map(_ swap)
  protected[this] val childrenByPosition = HashMap[NodePosition, ListBuffer[TreeNode]]()
  protected[this] val nodeListByLevel = HashMap[Int, ListBuffer[TreeNode]](0 -> ListBuffer(rootNode))

  /**
   * Make node and add a new node to a tree as a root node children
   * @param value node content
   * @return new TreeNode
   */
  def add(value: T): TreeNode = {
    add(value, rootNode)
  }

  /**
   * Make node and add a new node to a tree as children of an assigned node
   * @param value node content
   * @param parentNode a parent node of a new node
   * @return new node
   */
  def add(value: T, parentNode: TreeNode): TreeNode = {
    val node = ElementNode(value)
    addNode(node, parentNode)
    node
  }

  /**
   * Make node and add a new node to a tree as children of an assigned position
   * @param value node content
   * @param parentPosition a parent position of a new node
   * @return new node
   */
  def add(value: T, parentPosition: NodePosition): TreeNode = {
    val node = ElementNode(value)
    addNode(node, parentPosition)
    node
  }

  def getNode(position: NodePosition): Option[TreeNode] = {
    nodeByPosition.get(position)
  }

  /**
   * Add a new node to a tree as children of an assigned node
   * @param node new node
   * @param parentNode a parent node of a new node
   */
  protected def addNode(node: TreeNode, parentNode: TreeNode): Unit = {
    getPosition(parentNode).foreach { parentPosition =>
      val newLabel = tree.get(parentNode).map { nodeList =>
        nodeList += node
        nodeList.size - 1
      }.getOrElse {
        tree.put(parentNode, ListBuffer(node))
        0
      }
      addPosition(node, parentPosition.child(newLabel))
      addChild(node, parentPosition)
      addNodeListByLevel(node)
    }
  }


  /**
   * Add a new node to a tree as children of an assigned node
   * @param node new node
   * @param parentPosition a parent position of a new node
   */
  protected def addNode(node: TreeNode, parentPosition: NodePosition): Unit = {
    getNode(parentPosition).foreach { parentNode =>
      addNode(node, parentNode)
    }
  }

  def removeNode(node: TreeNode): Boolean = {
    positionByNode.get(node).exists { position =>
      getNode(position.parent).foreach { parentNode =>
        tree.get(parentNode).foreach { parentChildren =>
          parentChildren -= node
        }
      }
      tree.remove(node)
      removeNodeListByLevel(node)
      removeChild(node, position.parent)
      removePosition(node, position)
      true
    }
  }

  /**
   * Find a position of node
   * @param node Node
   * @return an optional value of a node position
   */
  def getPosition(node: TreeNode): Option[NodePosition] = {
    positionByNode.get(node)
  }


  /**
   * Add a position to a tree
   * @param node Node
   * @param position NodePosition
   */
  protected def addPosition(node: TreeNode, position: NodePosition): Unit = {
    positionByNode.put(node, position)
    nodeByPosition.put(position, node)
  }

  protected def removePosition(node: TreeNode, position: NodePosition): Unit = {
    positionByNode.remove(node)
    nodeByPosition.remove(position)
  }

  def getParent(position: NodePosition): Option[TreeNode] = {
    val parentPosition = position.parent
    nodeByPosition.get(parentPosition)
  }

  def getParent(node: TreeNode): Option[TreeNode] = {
    getPosition(node).map { position =>
      val parentPosition = position.parent
      nodeByPosition.get(parentPosition)
    }.getOrElse {
      None
    }
  }

  def getChildren(position: NodePosition): List[TreeNode] = {
    childrenByPosition.get(position).map { children =>
      children.toList
    }.getOrElse {
      List.empty[TreeNode]
    }
  }

  def getChildren(node: TreeNode): List[TreeNode] = {
    getPosition(node).map { position =>
      childrenByPosition.get(position).map { children =>
        children.toList
      }.getOrElse {
        List.empty[TreeNode]
      }
    }.getOrElse {
      List.empty[TreeNode]
    }
  }

  protected def addChild(node: TreeNode, position: NodePosition): Unit = {
    childrenByPosition.get(position).map { children =>
      children += node
    }.getOrElse {
      childrenByPosition.put(position, ListBuffer(node))
    }
  }

  protected def removeChild(node: TreeNode, position: NodePosition): Unit = {
    childrenByPosition.get(position).foreach { children =>
      children -= node
    }
  }

  def getSibling(node: TreeNode): List[TreeNode] = {
    getParent(node).map { parentNode =>
      getChildren(parentNode)
    }.getOrElse {
      List.empty[TreeNode]
    }
  }

  def getSibling(position: NodePosition): List[TreeNode] = {
    val parentPosition = position.parent
    getChildren(parentPosition)
  }

  def maxLevel: Int = nodeListByLevel.keys.max

  def getLevel(node: TreeNode): Int = {
    getPosition(node).map(_.level).getOrElse(0)
  }

  def getLevel(position: NodePosition): Int = {
    position.level
  }

  def getNodeListByLevel(level: Int): List[TreeNode] = {
    nodeListByLevel.get(level).map { nodeList =>
      nodeList.toList
    }.getOrElse {
      List.empty[TreeNode]
    }
  }

  protected def addNodeListByLevel(node: TreeNode): Unit = {
    positionByNode.get(node).foreach { position =>
      val level = position.level
      nodeListByLevel.get(level).map { nodeList =>
        nodeList += node
      }.getOrElse {
        nodeListByLevel.put(level, ListBuffer(node))
      }
    }
  }

  protected def removeNodeListByLevel(node: TreeNode): Unit = {
    positionByNode.get(node).foreach { position =>
      val level = position.level
      nodeListByLevel.get(level).foreach { nodeList =>
        nodeList -= node
      }
    }
  }

  def printNodeWithPosition(node: TreeNode): Unit = {
    positionByNode.get(node).foreach { position =>
      println(node, position)
    }
  }

  def display(): Unit = {
    (0 to maxLevel).foreach { level =>
      nodeListByLevel.get(level).foreach { nodeList =>
        println(s"level:$level [${nodeList.mkString(", ")}]")
      }
    }
  }
}

object Tree {
  def apply[T] = new Tree[T]
}

