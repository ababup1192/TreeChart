package org.ababup1192.layout

import org.ababup1192.util.graph.{ElementNode, NodePosition, Tree, TreeNode}

import scala.collection.mutable.{OpenHashMap => HashMap}
import scalafx.scene.Node

class DiagramTree extends Tree[Node] {
  var yAxisSpacing = 100d
  var xAxisSpacing = 100d

  protected var widthByNode = HashMap[TreeNode, Double](rootNode -> 0.0)
  protected var widthByPosition = HashMap[NodePosition, Double](NodePosition.ROOT -> 0.0)
  protected var heightByLevel = HashMap[Int, Double](0 -> 0.0)

  def getWidth(node: TreeNode): Option[Double] = {
    widthByNode.get(node)
  }

  def getWidth(node: NodePosition): Option[Double] = {
    widthByPosition.get(node)
  }

  def getHeight(level: Int): Option[Double] = {
    heightByLevel.get(level)
  }

  def calcLayout() = {
    widthByNode = HashMap[TreeNode, Double](rootNode -> 0.0)
    widthByPosition = HashMap[NodePosition, Double](NodePosition.ROOT -> 0.0)
    heightByLevel = HashMap[Int, Double](0 -> 0.0)

    (1 to maxLevel).foreach { level =>
      val currentLevelNodeList = getNodeListByLevel(level)

      calcWidthLayout(currentLevelNodeList)
      calcHeightLayout(level, currentLevelNodeList)
    }
  }

  protected def calcWidthLayout(nodeList: List[TreeNode]): Unit = {
    nodeList.scanLeft(0d) { (width, treeNode) =>
      // Set Width
      treeNode match {
        case ElementNode(node: Node) =>
          val nodeWidth = width + node.boundsInLocal.value.getWidth + xAxisSpacing
          widthByNode.put(treeNode, nodeWidth)
          getPosition(treeNode).foreach { position =>
            widthByPosition.put(position, nodeWidth)
          }
          nodeWidth
        case _ =>
          width + 0d
      }
    }
  }

  protected def calcHeightLayout(level: Int, nodeList: List[TreeNode]): Unit = {
    val maxHeight = nodeList.map {
      case ElementNode(node: Node) => node.boundsInLocal.value.getHeight
      case _ => 0d
    }.max
    val parentHeight = heightByLevel.getOrElse(level - 1, 0d)
    heightByLevel.put(level, parentHeight + maxHeight + yAxisSpacing)
  }
}
