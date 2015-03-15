package org.ababup1192.layout

import org.ababup1192.util.graph.{ElementNode, RootNode, TreeNode}

import scalafx.scene.Node
import scalafx.scene.layout.Pane

class TreePane extends Pane {

  minHeight.bind(prefHeight)
  maxHeight.bind(prefHeight)

  minWidth.bind(prefWidth)
  maxWidth.bind(prefWidth)

  val diagramTree = new DiagramTree

  def addChild(node: Node, parentNode: TreeNode = RootNode): TreeNode = {
    val newTreeNode = diagramTree.add(node, parentNode)
    children.add(node)
    layoutChildren()
    newTreeNode
  }

  def layoutChildren(): Unit = {
    diagramTree.calcLayout()
    (1 to diagramTree.maxLevel).foreach { level =>
      diagramTree.getNodeListByLevel(level).foreach {
        case treeNode@ElementNode(node: Node) =>
          (diagramTree.getWidth(treeNode), diagramTree.getHeight(level)) match {
            case (Some(width), Some(height)) =>
              node.relocate(width, height)
            case _ =>
          }
        case _ =>
      }
    }
  }

}
