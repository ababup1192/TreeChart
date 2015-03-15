package org.ababup1192.layout

import scalafx.Includes._
import scalafx.beans.property.DoubleProperty
import scalafx.scene.input.MouseEvent
import scalafx.scene.{Cursor, Node}

trait Draggable extends Node {
  protected val translateRefX = new DoubleProperty
  protected val translateRefY = new DoubleProperty

  protected var dragAnchorX: Double = _
  protected var dragAnchorY: Double = _

  protected var initTranslateX: Double = _
  protected var initTranslateY: Double = _

  cursor = Cursor.HAND

  translateX <== translateRefX
  translateY <== translateRefY

  onMousePressed = (mouseEvent: MouseEvent) => {
    initTranslateX = translateX()
    initTranslateY = translateY()

    dragAnchorX = mouseEvent.sceneX
    dragAnchorY = mouseEvent.sceneY

    this.toFront()
  }
  onMouseDragged = (mouseEvent: MouseEvent) => {
    val dragX = mouseEvent.sceneX - dragAnchorX
    val dragY = mouseEvent.sceneY - dragAnchorY

    translateRefX() = initTranslateX + dragX
    translateRefY() = initTranslateY + dragY
  }
}
