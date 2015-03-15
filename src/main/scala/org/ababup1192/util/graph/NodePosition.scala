package org.ababup1192.util.graph

/**
 * Tree path class
 * @param path Vector
 */
class NodePosition(val path: Vector[Int]) extends Ordered[NodePosition] {

  /**
   * Return a parent position
   * @return NodePosition
   */
  def parent: NodePosition = {
    if (path.isEmpty) {
      NodePosition.ROOT
    } else {
      NodePosition(path.init)
    }
  }

  /**
   * Return a child position
   * @param label a next child node's label
   * @return NodePosition
   */
  def child(label: Int): NodePosition = {
    val newPath = path :+ label
    NodePosition(newPath)
  }

  /**
   * Return a node level in tree
   * @return Int
   */
  def level: Int = {
    path.length
  }

  override def compare(other: NodePosition): Int = {
    if (level < other.level) {
      -1
    } else if (level > other.level) {
      1
    } else {
      val compareList = ((path zip other.path) map Function.tupled(_ compare _)).filter(_ != 0)
      // compare each element. return '0' if all elements equal.
      compareList.headOption.getOrElse(0)
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: NodePosition =>
        path == that.path
      case _ => false
    }
  }

  override def hashCode(): Int = {
    13 * 7 + path.hashCode()
  }

  override def toString: String = {
    s"Position(path=$path, level=$level)"
  }
}

object NodePosition {
  // ROOT is a empty path
  val ROOT = NodePosition()

  def apply() = new NodePosition(Vector[Int]())

  def apply(path: Vector[Int]) = new NodePosition(path)

  def apply(path: Int*) = new NodePosition(path.toVector)
}
