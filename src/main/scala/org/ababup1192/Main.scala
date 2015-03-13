package org.ababup1192

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Label, ScrollPane}

object Main extends JFXApp {

  stage = new PrimaryStage() {
    title = "Hello ScalaFX"
    val treePane = new TreePane()
    treePane.yAxisSpacingProperty() = 10d

    treePane.addChild(Label("ROOT"), NodePosition.ROOT)
    treePane.addChild(Label("0"), NodePosition.ROOT.child(0))

    treePane.addChild(Label("00"), NodePosition.ROOT.child(0).child(0))
    treePane.addChild(Label("000"), NodePosition.ROOT.child(0).child(0).child(0))

    treePane.addChild(Label("01"), NodePosition.ROOT.child(0).child(1))
    treePane.addChild(Label("02"), NodePosition.ROOT.child(0).child(2))
    treePane.addChild(Label("010"), NodePosition.ROOT.child(0).child(1).child(0))
    treePane.addChild(Label("011"), NodePosition.ROOT.child(0).child(1).child(1))

    /*
    treePane.addChild(PackageBox("ROOT"), NodePosition.ROOT)

    treePane.addChild(PackageBox("pack_a"), NodePosition.ROOT.child(0))
    treePane.addChild(PackageBox("pack_b"), NodePosition.ROOT.child(1))
    treePane.addChild(PackageBox("pack_b_a"), NodePosition.ROOT.child(1).child(0))
    treePane.addChild(PackageBox("pack_a_a"), NodePosition.ROOT.child(0).child(1))
    */

    val root = new ScrollPane {
      content = treePane
    }

    scene = new Scene(root, 900, 700)
    scene.value.getStylesheets.add(getClass.getResource("/style01.css").toString)

  }

}
