package org.ababup1192.layout

import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.Label
import scalafx.scene.layout.{StackPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

/**
 * PackageBox Class
 * @param packageName Package display name
 */
class PackageBox(packageName: String) extends VBox with Draggable {
  private[this] val topBox = new Rectangle with BasePackageBox {
    width = 30
    height = 20
  }

  // Main box's size is variable width
  private[this] val mainBox = new Rectangle with BasePackageBox {
    width = 100
    height = 65
  }

  private[this] val label = new Label(packageName) {
    padding = Insets(15, 0, 0, 0)
  }

  children = ObservableBuffer(
    // decoration box
    topBox,
    // main box
    new StackPane {
      alignment = Pos.TopCenter
      padding = Insets(-5, 0, 0, 0)

      children = Seq(
        mainBox, label
      )
    }
  )

  def changePackageName(packageName: String) = {
    label.text = packageName
  }

}

object PackageBox {

  def apply(packageName: String) = {
    new PackageBox(packageName)
  }
}

trait BasePackageBox extends Rectangle {
  val packageColor = Color.web("#b1cfe2")

  fill = packageColor
  stroke = Color.Black
  strokeWidth = 3d
}

