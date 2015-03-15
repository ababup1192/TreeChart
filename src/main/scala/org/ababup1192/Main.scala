package org.ababup1192

import org.ababup1192.layout.{PackageBox, TreePane}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.ScrollPane

object Main extends JFXApp {

  stage = new PrimaryStage() {
    title = "Hello ScalaFX"
    val treePane = new TreePane()

    val defaultPackage = treePane.addChild(PackageBox("DefaultPackage"))
    val hoge = treePane.addChild(PackageBox("hoge"), defaultPackage)
    treePane.addChild(PackageBox("foo"), hoge)
    treePane.addChild(PackageBox("bar"), hoge)

    val root = new ScrollPane {
      content = treePane
    }

    scene = new Scene(root, 900, 700)
    // scene.value.getStylesheets.add(getClass.getResource("/style01.css").toString)

  }

}
