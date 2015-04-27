package scala.slick.shadow

import scala.slick.typeproviders.codegenerator.Generator

class YYCodeGenerator(val inputModule: String) extends Generator {
  import scala.reflect.runtime.{ universe => runtimeUniverse }
  import runtimeUniverse._

  val runtimeMirror = runtimeUniverse.runtimeMirror(this.getClass.getClassLoader)

  val yyTranformers = new {
    val universe: runtimeUniverse.type = runtimeUniverse
  } with YYTransformers
  val classVirtualization = new yyTranformers.ClassVirtualization
  val (caseClassTables, caseClassSymbols) = {
    val module = runtimeMirror.staticModule(inputModule)
    val reflectedModule = runtimeMirror.reflectModule(module)
    val caseClassSymbols = module.typeSignature.members.filter(x => x.isClass && x.asClass.isCaseClass)
    val caseClassTables = caseClassSymbols.map(classVirtualization.getTableFromSymbol)
    (caseClassTables, caseClassSymbols)
  }

  val tableTrees = caseClassTables.zip(caseClassSymbols).flatMap { case (table, symbol) => classVirtualization.getTreesFromTable(table)(symbol) }

  def generateCode(className: String, packageName: Option[String]): String = {
    incIndent
    val tableCode = tableTrees map generateCodeForTable
    val importsCode = List(s"import $inputModule._", "import scala.language.implicitConversions")
    decIndent
    s"""
${if (packageName.isEmpty) "" else s"package ${packageName.get}"}
object $className extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes {
${importsCode.mkString("  ", "\n  ", "")}
    
${tableCode.mkString("\n")}
}   
"""
  }
}