package scala.slick.shadow.test

import scala.slick.shadow.VirtualizationMain

object YYClasses {
  def main(args: Array[String]) {
    val outputDir = args(0)
    VirtualizationMain.main(Array("scala.slick.shadow.test.YYDefinitions", outputDir, "scala.slick.shadow.VirtualizationModule"))
  }
}