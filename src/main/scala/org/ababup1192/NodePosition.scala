package org.ababup1192

class NodePosition(val path: Seq[Int]) extends Ordered[NodePosition] {

  def parent = {
    if (path.nonEmpty) {
      NodePosition(path.init)
    } else {
      NodePosition.ROOT
    }
  }

  def child(pathElement: Int): NodePosition = {
    val newPath = path :+ pathElement
    NodePosition(newPath)
  }

  def level = {
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
  val ROOT = NodePosition()

  def apply() = new NodePosition(Seq[Int]())

  def apply(path: Seq[Int]) = new NodePosition(path)
}