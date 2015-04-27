package scala.slick.typeproviders

import scala.slick.typeproviders.codegenerator.Generator

class CodeGenerator(val configFileName: String) extends Generator {
  import scala.reflect.runtime.{ universe => runtimeUniverse }
  import runtimeUniverse._

  val macroHelper = new {
    val universe: runtimeUniverse.type = runtimeUniverse
  } with MacroHelpers(DefaultContextUtils, configFileName)

  val tableTrees = macroHelper.generateTreeForTables
  val imports = macroHelper.getImports
  val connectionString = macroHelper.urlForConnection
  import macroHelper.{ userForConnection, passForConnection, slickDriverObject, jdbcClass }

  def generateCode(className: String = "GeneratedDb", packageName: Option[String] = None): String = {
    incIndent
    val tableCode = tableTrees map generateCodeForTable
    val importsCode = imports map generateCodeForImport
    val threeQ = "\"\"\""
    decIndent
    s"""
${if (packageName.isEmpty) "" else s"package ${packageName.get}"}
object $className {
${importsCode.mkString("\n")}
    
  val driver = $slickDriverObject
  val database = driver.simple.Database.forURL($threeQ$connectionString$threeQ, driver = "$jdbcClass",
                                               user = "$userForConnection", password = "$passForConnection")

${tableCode.mkString("\n")}
}   
"""
  }
}

object DefaultContextUtils {
  var map = new collection.mutable.HashMap[String, Int]
  def freshName(name: String): String = {
    val index = map.get(name) match {
      case Some(i) => {
        map(name) = i + 1
        i
      }
      case None => {
        map(name) = 1
        0
      }
    }
    name + "$" + index
  }
}
