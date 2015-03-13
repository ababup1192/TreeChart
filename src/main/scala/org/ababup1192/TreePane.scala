package org.ababup1192

import java.lang
import javafx.beans.value.{ChangeListener, ObservableValue}

import scala.collection.mutable.{HashMap => MHashMap, HashSet => MHashSet, Map => MMap, Set => MSet}
import scalafx.beans.property.{BooleanProperty, DoubleProperty}
import scalafx.geometry.{Point2D, Rectangle2D}
import scalafx.scene.Node
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Line

class TreePane extends Pane {
  val yAxisSpacing = 200d
  val xAxisSpacing = 200d
  val lineSpacing = 2d
  var isShowLines = true

  val yAxisSpacingProperty: DoubleProperty = new DoubleProperty()
  val xAxisSpacingProperty: DoubleProperty = new DoubleProperty()
  val lineSpacingProperty: DoubleProperty = new DoubleProperty()
  val showLinesProperty: BooleanProperty = new BooleanProperty()

  // style = "tree-pane"

  minHeight.bind(prefHeight)
  maxHeight.bind(prefHeight)

  minWidth.bind(prefWidth)
  maxWidth.bind(prefWidth)


  private val spacingChangeListener = new ChangeListener[Number] {
    override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit = {
      layoutChildren()
    }
  }

  private val showLinesChangeListener = new ChangeListener[java.lang.Boolean] {
    override def changed(observable: ObservableValue[_ <: lang.Boolean], oldValue: lang.Boolean, newValue: lang.Boolean): Unit = {
      layoutChildren()
    }
  }

  yAxisSpacingProperty.addListener(spacingChangeListener)
  xAxisSpacingProperty.addListener(spacingChangeListener)
  lineSpacingProperty.addListener(spacingChangeListener)

  showLinesProperty.addListener(showLinesChangeListener)

  private val nodeByPosition: MMap[NodePosition, Node] = new MHashMap[NodePosition, Node]()
  private val positionByNode: MMap[Node, NodePosition] = new MHashMap[Node, NodePosition]()
  private val nodeByParentPosition: MMap[NodePosition, MSet[Node]] = new MHashMap[NodePosition, MSet[Node]]()
  private val nodeByLevel: MMap[Integer, MSet[Node]] = new MHashMap[Integer, MSet[Node]]

  var lines = Seq[Line]()

  def layoutChildren() = {

    if (nodeByPosition.isEmpty) {
      adjustLineCount(0)
      prefWidth = 0
      prefHeight = 0
    } else {

      val widthByPosition = new MHashMap[NodePosition, Double]
      val levelHeight = new MHashMap[Int, Double]
      val positionsByLevel = new MHashMap[Int, MSet[NodePosition]]
      val positionsByParentPosition = new MHashMap[NodePosition, MSet[NodePosition]]

      val maxLevel = nodeByLevel.keys.max.toInt

      (0 to maxLevel).reverse.foreach { level =>
        levelHeight.put(level, 0d)
        positionsByLevel.put(level, MSet[NodePosition]())
      }


      (0 to maxLevel).reverse.foreach { level =>
        nodeByLevel.get(level).foreach { nodes =>
          nodes.foreach { node =>
            positionByNode.get(node).foreach { position =>
              val nodeBounds = node.layoutBounds
              widthByPosition.put(position, nodeBounds.value.getWidth + this.xAxisSpacing)
              levelHeight.put(level, Math.max(levelHeight.get(level).get, nodeBounds.value.getHeight + this.yAxisSpacing))

              positionsByLevel.get(level).foreach(_.add(position))
              if (level > 0) {
                positionsByLevel.get(level - 1).foreach(_.add(position.parent))
              }
            }
          }
        }

        positionsByLevel.get(level).foreach(positions =>
          positions.foreach { position =>
            if (position.level > 0) {
              val parentPosition = position.parent
              positionsByLevel.get(position.level - 1).foreach(_.add(parentPosition))
              positionsByParentPosition.get(parentPosition).map {
                _.add(position)
              }.getOrElse {
                positionsByParentPosition.put(parentPosition, MSet(position))
              }
            }

            var widthOfChildren = 0d
            positionsByParentPosition.get(position).foreach {
              parentPositions =>
                parentPositions.foreach { childPosition =>
                  widthByPosition.get(childPosition).foreach { width =>
                    widthOfChildren += width
                  }
                }
            }
            widthByPosition.get(position).map { width =>
              widthByPosition.put(position, Math.max(width, widthOfChildren))
            }.getOrElse {
              widthByPosition.put(position, widthOfChildren)
            }
          }
        )
      }
      val boxesByPosition = new MHashMap[NodePosition, Rectangle2D]()
      boxesByPosition.put(NodePosition.ROOT, new Rectangle2D(0d, 0d, widthByPosition.get(NodePosition.ROOT).get, levelHeight.get(0).get))

      (0 to maxLevel).foreach { level =>
        positionsByLevel.get(level).foreach { positions =>
          positions.foreach { position =>
            val positionBox = boxesByPosition(position)
            val childPositions = positionsByParentPosition.getOrElse(position, List()).toList.sorted
            var childX = positionBox.getMinX
            childPositions.foreach { childPosition =>
              widthByPosition.get(childPosition).foreach { childWidth =>
                boxesByPosition.put(childPosition, new Rectangle2D(childX, positionBox.getMaxY, childWidth, levelHeight(childPosition.level)))
                childX += childWidth
              }
            }
          }
        }
      }
      val xCenterHintByPosition = new MHashMap[NodePosition, Double]
      val yCenterHintByPosition = new MHashMap[NodePosition, Double]

      (0 to maxLevel).reverse.foreach { level =>
        positionsByLevel.get(level).foreach {
          _.foreach { position =>
            val positionBox = boxesByPosition(position)
            val xCenterHint = xCenterHintByPosition.getOrElse(position, (positionBox.getMinX + positionBox.getMaxX) / 2d)
            val yCenterHint = (positionBox.getMinY + positionBox.getMaxY) / 2d

            xCenterHintByPosition.put(position, xCenterHint)
            yCenterHintByPosition.put(position, yCenterHint)
            nodeByPosition.get(position).foreach { node =>
              val nodeBounds = node.layoutBounds.value
              node.relocate(xCenterHint - nodeBounds.getWidth / 2d, yCenterHint - nodeBounds.getHeight / 2d)
            }
            val parentPosition = position.parent
            xCenterHintByPosition.get(parentPosition).map { parentXCenterHint =>
              xCenterHintByPosition.put(parentPosition, (parentXCenterHint + xCenterHint) / 2d)
            }.getOrElse {
              xCenterHintByPosition.put(parentPosition, xCenterHint)
            }
          }
        }
      }
      if (this.isShowLines) {
        adjustLineCount(boxesByPosition.size - 1)

        var currentLine = 0
        boxesByPosition.keys.foreach { position =>
          positionsByParentPosition.get(position).foreach { childPositions =>
            childPositions.foreach { childPosition =>
              // println(nodeByPosition.get(position).get.layoutBounds)
              val fromBounds = nodeByPosition.get(position).map(node => Some(node.boundsInLocal.value)).getOrElse(None)
              val toBounds = nodeByPosition.get(childPosition).map(node => Some(node.boundsInLocal.value)).getOrElse(None)
              val lineFrom = new Point2D(
                xCenterHintByPosition(position) + fromBounds.map(_.getWidth / 2d).getOrElse(0d),
                yCenterHintByPosition(position) + fromBounds.map(_.getHeight / 2d).getOrElse(0d) + this.lineSpacing
              )

              val lineTo = new Point2D(
                xCenterHintByPosition(childPosition) + toBounds.map(_.getWidth / 2d).getOrElse(0d),
                yCenterHintByPosition(childPosition) + toBounds.map(_.getHeight / 2d).getOrElse(0d) + this.lineSpacing
              )
              try {
                val line = lines(currentLine)

                line.setStartX(lineFrom.getX)
                line.setStartY(lineFrom.getY)

                line.setEndX(lineTo.getX)
                line.setEndY(lineTo.getY)

                currentLine += 1
              } catch {
                case e: Exception =>
              }
            }
          }
        }
      } else {
        adjustLineCount(0)
      }
      val totalHeight = levelHeight.values.sum
      prefWidth = widthByPosition.get(NodePosition.ROOT).get
      prefHeight = totalHeight
    }

  }

  def adjustLineCount(count: Int) = {
    while (count < lines.size) {
      children.remove(lines.size - 1)
      // this.parent.value.getChildrenUnmodifiable.remove(lines.last)
      lines = lines.dropRight(1)
    }

    while (count > lines.size) {
      val line = new Line {
        // style = "tree-pane-line"
      }

      children.add(line)
      // this.parent.value.getChildrenUnmodifiable.add(line)
      line.toBack()
      lines = lines :+ line
    }
  }

  def addChild(node: Node, position: NodePosition) {
    if (!this.getChildrenUnmodifiable.contains(node)) {
      children.add(node)
      // parent.value.getChildrenUnmodifiable.add(node)
      node.toFront()
    }
    setPosition(node, position)
    layoutChildren()
  }

  def removeChild(node: Node) {
    unsetPosition(node)
    this.getChildrenUnmodifiable.remove(node)
    this.parent.value.getChildrenUnmodifiable.remove(node)
    layoutChildren()
  }

  def getPosition(node: Node): Option[NodePosition] = positionByNode.get(node)

  def getNode(position: NodePosition): Option[Node] = nodeByPosition.get(position)

  def getNodesOfParent(position: NodePosition): Option[Set[Node]] = {
    nodeByParentPosition.get(position) match {
      case Some(nodes) => Some(Set.empty ++ nodes)
      case None => None
    }
  }

  def getNodesOfLevel(level: Int): Option[Set[Node]] = {
    nodeByLevel.get(level) match {
      case Some(nodes) => Some(Set.empty ++ nodes)
      case None => None
    }
  }

  protected def setPosition(node: Node, position: NodePosition) {
    unsetPosition(node)
    nodeByPosition.get(position).map { n =>
      unsetPosition(n)
    }

    val parentPosition: NodePosition = position.parent
    val level: Int = position.level
    positionByNode.put(node, position)
    nodeByPosition.put(position, node)

    nodeByParentPosition.get(parentPosition).map {
      _.add(node)
    }.getOrElse {
      nodeByParentPosition.put(parentPosition, MHashSet(node))
    }

    nodeByLevel.get(level).map {
      _.add(node)
    }.getOrElse {
      nodeByLevel.put(level, MHashSet(node))
    }
  }

  protected def unsetPosition(node: Node) {
    positionByNode.get(node).map { position =>
      positionByNode.remove(node)
      nodeByPosition.remove(position)
      nodeByParentPosition.get(position.parent).map(_.remove(node))
      nodeByLevel.get(position.level).map(_.remove(node))
    }

  }
}
